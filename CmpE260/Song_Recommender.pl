% kiymet akdemir
% 2019400252
% compiling: yes
% complete: yes
features([explicit-0, danceability-1, energy-1,
          key-0, loudness-0, mode-1, speechiness-1,
       	  acousticness-1, instrumentalness-1,
          liveness-1, valence-1, tempo-0, duration_ms-0,
          time_signature-0]).
features.
filter_features(Features, Filtered) :- features(X), filter_features_rec(Features, X, Filtered).
filter_features_rec([], [], []).
filter_features_rec([FeatHead|FeatTail], [Head|Tail], FilteredFeatures) :-
    filter_features_rec(FeatTail, Tail, FilteredTail),
    _-Use = Head,
    (
        (Use is 1, FilteredFeatures = [FeatHead|FilteredTail]);
        (Use is 0,
            FilteredFeatures = FilteredTail
        )
    ).
getArtistTracks(ArtistName,TrackIds,TrackNames):-	%gets all tracks of given artist name (pred1)
    artist(ArtistName,_,AlbumIds),
    getAlbums(AlbumIds,ArtistName,TrackIds),
    getTracks(TrackIds,TrackNames).
getAlbums([],_,[]).
getAlbums([AlHead|AlTail],ArtistName,TrackIds):-	%gets all tracks of given album list
    album(AlHead,_,_,TrackIds1),
    getAlbums(AlTail,ArtistName,TrackIds2),
    append(TrackIds1,TrackIds2,TrackIds).
getTracks([],[]).
getTracks([TrHead|TrTail],TrackNames):-	%gets track names of given track ids as list
    track(TrHead,TrackNames1,_,_,_),
    getTracks(TrTail,TrackNames2),
    append([TrackNames1],TrackNames2,TrackNames).
albumFeatures(AlbumId,AlbumFeatures):-		%features of tracks in the given album id	(pred2)
    album(AlbumId,_,_,TrackIds),
    getFeatures(TrackIds,AlbumFeatures).
getFeatures([],[0,0,0,0,0,0,0,0]).
getFeatures([TrHead|TrTail],AverageofFeatures):-	%evaluates features of given trackid list
    track(TrHead,_,_,_,Features),
    filter_features(Features,Filtered),
    getFeatures(TrTail,AverageofFeatures2),
    length(TrTail,L),
    average(L,Filtered,AverageofFeatures2,AverageofFeatures).
average(_,[],[],[]).
average(Length,[NewElement1|NewElementT],[List1|ListT],[Average1|AverageT]):-	%calculates the average of each index in the list
    Average1 is (NewElement1+Length*List1)/(Length+1),
    average(Length,NewElementT,ListT,AverageT).
artistFeatures(ArtistName,ArtistFeatures):-		%evaluates the features of tracks of an artist	(pred3)
    getArtistTracks(ArtistName,TrackIds,_),
    getFeatures(TrackIds,ArtistFeatures).
trackDistance(TrackId1,TrackId2,Score):-	%gets the distance between two tracks	(pred4)
    track(TrackId1,_,_,_,Features1),
    track(TrackId2,_,_,_,Features2),
    filter_features(Features1,Filtered1),
    filter_features(Features2,Filtered2),
    find_distance(Filtered1,Filtered2,Score).
find_distance([],[],0).
find_distance([Ft1Head|Ft1Tail],[Ft2Head|Ft2Tail],Distance):-	%evaluates the distance the distance of given two features list
    abs((Ft1Head-Ft2Head), Sq1),
    find_distance(Ft1Tail,Ft2Tail,Sqtail),
    sqrt(Sq1*Sq1+Sqtail*Sqtail,Distance).
albumDistance(AlbumId1,AlbumId2,Score):-	%gets the distance between two albums	(pred5)
    albumFeatures(AlbumId1,Features1),
    albumFeatures(AlbumId2,Features2),
    find_distance(Features1,Features2,Score).
artistDistance(Artist1,Artist2,Score):-		%gets the distance between two artist	(pred6)
    artistFeatures(Artist1,Features1),
    artistFeatures(Artist2,Features2),
    find_distance(Features1,Features2,Score).
findMostSimilarTracks(TrackId,SimilarIds,SimilarNames):-	%gets thirty tracks which have smallest distances to given track	(pred7)
    findall(TrId, track(TrId,_,_,_,_), Bag),
    pairBagTrack(TrackId,Bag,List),
    keysort(List, [_|Sorted]),
    getThirty(0,Sorted,SimilarIds),
    getNamesTrack(SimilarIds,SimilarNames).
findMostSimilarAlbums(AlbumId,SimilarIds,SimilarNames):-	%gets thirty tracks which have smallest distances to given album	(pred8)
    findall(AlId, album(AlId,_,_,_), Bag),
    pairBagAlbum(AlbumId,Bag,List),
    keysort(List, [_|Sorted]),
    getThirty(0,Sorted,SimilarIds),
    getNamesAlbum(SimilarIds,SimilarNames).
findMostSimilarArtists(ArtistName,SimilarNames):-	%gets thirty tracks which have smallest distances to given artist	(pred9)
    findall(AlId, artist(AlId,_,_), Bag),
    pairBagArtist(ArtistName,Bag,List),
    keysort(List, [_|Sorted]),
    getThirty(0,Sorted,SimilarNames).
pairBagTrack(_,[],[]).
pairBagTrack(TrackId,[Head|Tail],[DistanceHead|DistanceTail]):-		%pairs all tracks with their distances to given track
    trackDistance(TrackId,Head,Score),
    DistanceHead=Score-Head,
    pairBagTrack(TrackId,Tail,DistanceTail).
pairBagAlbum(_,[],[]).
pairBagAlbum(AlbumId,[Head|Tail],[DistanceHead|DistanceTail]):-		%pairs all tracks with their distances to given album
    albumDistance(AlbumId,Head,Score),
    DistanceHead=Score-Head,
    pairBagAlbum(AlbumId,Tail,DistanceTail).
pairBagArtist(_,[],[]).
pairBagArtist(ArtistId,[Head|Tail],[DistanceHead|DistanceTail]):-	%pairs all tracks with their distances to given artist
    artistDistance(ArtistId,Head,Score),
    DistanceHead=Score-Head,
    pairBagArtist(ArtistId,Tail,DistanceTail).
getThirty(K,[(_-IdHead)|Tail],[IdHead|SimilarTail]):-	%gets the second pairs of first thirty elements of given list
    (K<29,
    L is K+1,
    getThirty(L,Tail,SimilarTail));
    (K=29, SimilarTail=[]).
getNamesTrack([],[]).
getNamesTrack([IdHead|IdTail],[NameHead|NameTail]):-	%gets the names of given track ids
    track(IdHead,NameHead,_,_,_),
    getNamesTrack(IdTail,NameTail).
getNamesAlbum([],[]).
getNamesAlbum([IdHead|IdTail],[NameHead|NameTail]):-	%gets the names of given album ids
    album(IdHead,NameHead,_,_),
    getNamesAlbum(IdTail,NameTail).
filterExplicitTracks([],[]).
filterExplicitTracks([Head|Tail],Result):-	%filter the tracks which have explicit feature as 1	(pred10)
    track(Head,_,_,_,[Features1|_]),
    (Features1=0,
    filterExplicitTracks(Tail,Result1),
    append([Head],Result1,Result));
    (Features1=1,
    filterExplicitTracks(Tail,Result)).
getTrackGenre(TrackId,Genres):-		%gets the genres of artist of given track	(pred11)
    track(TrackId,_,Artists,_,_),
    getGenres(Artists,Genres1),
    sort(Genres1,Genres).
getGenres([],[]).
getGenres([Head|Tail],Genres):-		%gets the genres of given artist
    artist(Head,Genres1,_),
    getGenres(Tail,Genres2),
    append(Genres1,Genres2,Genres).
discoverPlaylist(LikedGenres,DislikedGenres,Features,FileName,Playlist):-	%gets the first thirty tracks which have smallest distances to given features
    findall(TrId, track(TrId,_,_,_,_), Bag),								%prints the playlist, track names, artists and distances to given features
    getLiked(LikedGenres,Bag,List1),										%(pred12)
    deleteDisliked(DislikedGenres,List1,List2),
    findMostSimilarTracks2(List2,Features,Playlist,TrackNames,Distances),
    findArtists(Playlist,Artists),
    open(FileName, write, Stream), writeln(Stream, Playlist),writeln(Stream, TrackNames),
    writeln(Stream, Artists),writeln(Stream, Distances), close(Stream).
getLiked([],_,[]).
getLiked([Head|Genres],Tracks,Result):-		%gets the tracks which have at least one genre from the given track list
    getLikedTracks(Head,Tracks,Result1),
    getLiked(Genres,Tracks,Result2),
    append(Result1,Result2,Result3),
    sort(Result3,Result).
getLikedTracks(_,[],[]).
getLikedTracks(Genre,[TrackId|Tail],List):-	%gets the tracks which have specific genre
    getTrackGenre(TrackId,TrackGenres),
    (((member(Genre, TrackGenres);
    substr(Genre,TrackGenres)),
    getLikedTracks(Genre,Tail,Result1),
    append([TrackId],Result1,Result),
    sort(Result,List));
    (\+member(Genre, TrackGenres),
    \+substr(Genre,TrackGenres),
    getLikedTracks(Genre,Tail,List))).
substr(Genre,[Head|Tail]):-		%checks if given genre is a substring of any element of genre list
    sub_string(Head, _, _, _, Genre);
    substr(Genre,Tail).
deleteDisliked([],List,List).
deleteDisliked([Head|Genres],Tracks,Liked):-	%deletes the tracks which have disliked genres from the given track list
    deleteDislikedGenre(Head,Tracks,Liked1),
    deleteDisliked(Genres,Liked1,Liked).
deleteDislikedGenre(_,[],[]).
deleteDislikedGenre(Genre,[TrackId|Tail],Result):-	%deletes the tracks which have specific genre
    getTrackGenre(TrackId,TrackGenres),
    (((member(Genre, TrackGenres);
    substr(Genre,TrackGenres)),
    deleteDislikedGenre(Genre,Tail,Result));
    (\+member(Genre, TrackGenres),
    \+substr(Genre,TrackGenres),
    deleteDislikedGenre(Genre,Tail,Result1),
    append([TrackId],Result1,Result))).
findMostSimilarTracks2(TrackIds,Features,SimilarIds,SimilarNames,Distances):-	%gets first thirty tracks which have smallest distances to given features
    pairTrack(TrackIds,Features,List),											%takes the ids, names and the distances of track list
    keysort(List, Sorted),
    getThirty(0,Sorted,SimilarIds),
    getThirtyDistances(0,Sorted,Distances),
    getNamesTrack(SimilarIds,SimilarNames).
pairTrack([],_,[]).
pairTrack([TrackId|Tail],Features,[DistanceHead|DistanceTail]):-	%gets all distances of tracks to given features
    track(TrackId,_,_,_,Features1),
    filter_features(Features1,Filtered1),
    find_distance(Filtered1,Features,Score),
    DistanceHead=Score-TrackId,
    pairTrack(Tail,Features,DistanceTail).
getThirtyDistances(K,[(Distance-_)|Tail],[Distance|DistTail]):-		%gets the first pairs(distances) of first thirty elements of given list
    (K<29,
    L is K+1,
    getThirtyDistances(L,Tail,DistTail));
    (K=29, DistTail=[]).
findArtists([],[]).
findArtists([Track|TrackTail],[Artist|ArtTail]):-	%gets the artists of given tracks
    track(Track,_,_,AlbumName,_),
    album(_,AlbumName,Artist,_),
    findArtists(TrackTail,ArtTail).