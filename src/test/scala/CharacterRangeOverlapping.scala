import iguana.regex.CharacterRange
import org.scalatest.FunSuite

import iguana.regex._


class CharacterRangeOverlapping extends FunSuite {

  /**
    * a----f
    *   d-----k
    */
  test("overlapping 1") {
    val r1 = 'a'--'f'
    val r2 = 'd'--'k'
    assert(r1.overlaps(r2))
    assert(r2.overlaps(r1))
  }

  /**
    * a-------z
    *    s--u
    */
  test("overlapping 2") {
    val r1 = 'a'--'z'
    val r2 = 's'--'u'
    assert(r1.overlaps(r2))
    assert(r2.overlaps(r1))
  }

  /**
    * a--f
    *       s--u
    */
  test("overlapping 3") {
    val r1 = 'a'--'f'
    val r2 = 's'--'u'
    assert(!r1.overlaps(r2))
    assert(!r2.overlaps(r1))
  }

  /**
    * a--f
    *    f--u
    */
  test("overlapping 4") {
    val r1 = 'a'--'f'
    val r2 = 'f'--'u'
    assert(r1.overlaps(r2))
    assert(r2.overlaps(r1))
  }

  test("overlapping to non-overlapping 1") {
    // 1-5 3-7
    val nonOverlapping = CharacterRange.toNonOverlappingMap(1--5, 3--7)

    // 1-2 3-5 6-7
    val expected = Map(1--5 -> Set(1--2, 3--5),
                       3--7 -> Set(3--5, 6--7))

    assertResult(expected)(nonOverlapping)
  }

  test("overlapping to non-overlapping 2") {
    // 1-7 5-13 6-12 17-21
    val nonOverlapping = CharacterRange.toNonOverlappingMap(1--7, 5--13, 17--21, 6--12)

    // 1-4 5-5 6-7 8-12 13-13 17-21
    val expected = Map(1--7   -> Set(1--4, 5--5, 6--7),
                       5--13  -> Set(5--5, 6--7, 8--12, 13--13),
                       6--12  -> Set(6--7, 8--12),
                       17--21 -> Set(17--21))

    assertResult(expected)(nonOverlapping)
  }

  test("overlapping to non-overlapping 3") {
    // 1-7 3-5 4-4
    val nonOverlapping = CharacterRange.toNonOverlappingMap(1--7, 3--5, 4--4)

    // 1-2 3-3 4-4 5-5 6-7
    val expected = Map(1--7 -> Set(1--2, 3--3, 4--4, 5--5, 6--7),
                       3--5 -> Set(3--3, 4--4, 5--5),
                       4--4 -> Set(4--4))

    assertResult(expected)(nonOverlapping)
  }

  test("overlapping to non-overlapping 4") {
    // 11-12 1-3 5-7
    val nonOverlapping = CharacterRange.toNonOverlappingMap(11--12, 1--3, 5--7)

    // 1-3 5-7 11-12
    val expected = Map(11--12 -> Set(11--12), 1--3 -> Set(1--3), 5--7 -> Set(5--7))

    assertResult(expected)(nonOverlapping)
  }

  test("overlapping to non-overlapping 5") {
    // 7-9 4-11 4-10 1-12 3-6 1-2
    val nonOverlapping = CharacterRange.toNonOverlappingMap(7--9, 4--11, 4--10, 1--12, 3--6, 1--2)

    // 1-2 3-3 4-6 7-9 10-10 11-11 12-12
    val expected = Map(7--9  -> Set(7--9),
                       4--11 -> Set(4--6, 7--9, 10--10, 11--11),
                       4--10 -> Set(4--6, 7--9, 10--10),
                       1--12 -> Set(1--2, 3--3, 4--6, 7--9, 10--10, 11--11, 12--12),
                       3--6  -> Set(3--3, 4--6),
                       1--2  -> Set(1--2))

    assertResult(expected)(nonOverlapping)
  }


}
