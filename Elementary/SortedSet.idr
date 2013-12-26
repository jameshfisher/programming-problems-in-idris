module SortedSet

import SortedMap

-- TODO: add intersection, union, difference

data SortedSet k = SetWrapper (SortedMap.SortedMap k ())

empty : SortedSet k
empty = SetWrapper SortedMap.empty

insert : Ord k => k -> SortedSet k -> SortedSet k
insert k (SetWrapper m) = SetWrapper (SortedMap.insert k () m)

delete : Ord k => k -> SortedSet k -> SortedSet k
delete k (SetWrapper m) = SetWrapper (SortedMap.delete k m)

contains : Ord k => k -> SortedSet k -> Bool
contains k (SetWrapper m) = isJust (SortedMap.lookup k m)

fromList : Ord k => List k -> SortedSet k
fromList l = SetWrapper (SortedMap.fromList (map (\i => (i, ())) l))

toList : Ord k => SortedSet k -> List k
toList (SetWrapper m) = map (\(i, _) => i) (SortedMap.toList m)
