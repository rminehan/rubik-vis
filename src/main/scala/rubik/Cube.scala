package rubik

import rubik.Color._

/** A cube is 6 faces with an assumed viewing orientation on each.
  *
  * Face n has color n at its center, e.g. face 3 has color at its center.
  *
  * TODO - move this to markdown docs.
  * The "orientation" of each face is found by starting on the middle square of face 0 and moving upwards
  * to the top middle square (pos1).
  * Then cross over onto the next face (face 1) moving towards the middle square.
  * Turn left at the middle and walk until hitting the edge.
  * That vector from the middle to the edge defines "up" for face 0.
  * It means that the square where face1 was entered is pos 7, and the square where is about to leave is pos 1.
  *
  * Repeat the above but on face 2, turn _right_ when you hit the middle.
  * Alternate the direction you turn on each face when hitting the center square,
  * otherwise you'll end up only traversing 3 faces.
  *
  * Eventually you'll end up back at face 0 entering it from pos 3 (middle right) and forming a loop.
  *
  * On all faces, the path leaves the face at pos 1, but enters at either pos 3 or 7.
  * Ones that enter at pos 3 are "right handed" because you "turn right" when hitting the center.
  * Ones that enter at pos 7 are "left handed".
  * Even indexed faces are right handed (faces 0, 2, 4).
  * Odd indexed faces are left handed (faces 1, 3, 5).
  * Internally I'll call this "chirality" or "handed-ness"
  *
  * The choise to make face 0 right handed is arbitrary (and reflects the right handed bias prevalent in our society),
  * keep those south paws in their place!
  */
case class Cube(
  face0: Face,
  face1: Face,
  face2: Face,
  face3: Face,
  face4: Face,
  face5: Face
)

object Cube {
  /** A solved cube has the correct color at all positions. */
  val solved: Cube = {
    def fill(color: Color): Face = Face(
      pos0 = color,
      pos1 = color,
      pos2 = color,
      pos3 = color,
      pos4 = color,
      pos5 = color,
      pos6 = color,
      pos7 = color
    )
    Cube(
      face0 = fill(Color0),
      face1 = fill(Color1),
      face2 = fill(Color2),
      face3 = fill(Color3),
      face4 = fill(Color4),
      face5 = fill(Color5)
    )
  }

  /** Returns a copy of the input cube where the face whose centre has the color passed
    * was rotated 90 degrees when viewed looking directly at the face.
    *
    * On the rotated face, top left moves to top right, top middle moves to right middle, etc...
    * Effectively each pos -> pos+2 % 8
    *
    * The adjacent faces are also modified along the connecting edges.
    * To understand those transformations requires understanding the coordinate system used to describe the cube.
    *
    * The below diagrams are "net" diagrams where the face i being rotated is at the center, and the 4 connected faces
    * are folded out into the 2D plane. The opposite face is not affected by the rotation and isn't included in the diagram.
    * The number at the center of each phase is defined relative to face i being rotated and reflects the ordering faces are traversed
    * from the perspective of the face being rotated.
    * The chirality of face i affects the coordinate system so there's sets of diagrams.
    *
    * Before rotation:
    *
    *                 Right Handed                                             Left Handed
    *
    *                 -------------                                            -------------
    *                |2     3     4|                                          |6     7     0|
    *                |             |                                          |             |
    *                |1    i+1    5|                                          |5    i+1    1|
    *                |             |                                          |             |
    *                |0     7     6|                                          |4     3     2|
    *                 -------------                                            -------------
    *  -------------  -------------  -------------              -------------  -------------  -------------
    * |2     3     4||0     1     2||2     3     4|            |6     7     0||0     1     2||6     7     0|
    * |             ||             ||             |            |             ||             ||             |
    * |1    i+2    5||7     i     3||1    i-1    5|            |5    i-1    1||7     i     3||5    i+2    1|
    * |             ||             ||             |            |             ||             ||             |
    * |0     7     6||6     5     4||0     7     6|            |4     3     2||6     5     4||4     3     2|
    *  -------------  -------------  -------------              -------------  -------------  -------------
    *                 -------------                                            -------------
    *                |6     7     0|                                          |2     3     4|
    *                |             |                                          |             |
    *                |5    i-2    1|                                          |1    i-2    5|
    *                |             |                                          |             |
    *                |4     3     2|                                          |0     7     6|
    *                 -------------                                            -------------
    *
    *
    * After rotation:

    *                 -------------                                            -------------
    *                |2     3     4|                                          |6     7     0|
    *                |             |                                          |             |
    *                |1    i+1    5|                                          |5    i+1    1|
    *                |-------------|                                          |-------------|
    *      from i-2  |6     5     4| <---- from i+2                 from i-2  |2     1     0| <---- from i-1
    *            \/   -------------                                       \/   -------------
    *  -------------  -------------  -------------              -------------  -------------  -------------
    * |2     3  |  6||6     7     0||0  |  3     4|            |6     7  |  2||6     7     0||4  |  7     0|
    * |         |   ||             ||   |         |            |         |   ||             ||   |         |
    * |1    i+2 |  7||5     i     1||7  | i-1    5|            |5    i-1 |  3||5     i     1||3  | i+2    1|
    * |         |   ||             ||   |         |            |         |   ||             ||   |         |
    * |0     7  |  0||4     3     2||6  |  7     6|            |4     3  |  4||4     3     2||2  |  3     2|
    *  -------------  -------------  -------------              -------------  -------------  -------------
    *                 -------------  ^ from i+1                                -------------  ^ from i+1
    * from i-1 ----> |0     1     2|                           from i+2 ----> |4     5     6|
    *                |-------------|                                          |-------------|
    *                |5    i-2    1|                                          |1    i-2    5|
    *                |             |                                          |             |
    *                |4     3     2|                                          |0     7     6|
    *                 -------------                                            -------------
    */
  def rotateClockwise(cube: Cube, color: Color): Cube = {
    val oldFacesSeq = Seq(
      cube.face0,
      cube.face1,
      cube.face2,
      cube.face3,
      cube.face4,
      cube.face5
    )

    val colorIndex = color match {
      case Color0 => 0
      case Color1 => 1
      case Color2 => 2
      case Color3 => 3
      case Color4 => 4
      case Color5 => 5
    }

    val oldFaceI = oldFacesSeq(colorIndex)

    def relativeIndex(offset: Int): Int = (colorIndex + offset + 6) % 6

    val oldConnectedFaces = ConnectedFaces(
      minus2 = oldFacesSeq(relativeIndex(-2)),
      minus1 = oldFacesSeq(relativeIndex(-1)),
      plus1 = oldFacesSeq(relativeIndex(1)),
      plus2 = oldFacesSeq(relativeIndex(2))
    )

    val newFaceI = Face(
      pos0 = oldFaceI.pos6,
      pos1 = oldFaceI.pos7,
      pos2 = oldFaceI.pos0,
      pos3 = oldFaceI.pos1,
      pos4 = oldFaceI.pos2,
      pos5 = oldFaceI.pos3,
      pos6 = oldFaceI.pos4,
      pos7 = oldFaceI.pos5
    )

    val ConnectedFaces(newFaceIMinus2, newFaceIMinus1, newFaceIPlus1, newFaceIPlus2) = colorIndex % 2 match {
      case 0 => rotateRightChirality(oldConnectedFaces)
      case 1 => rotateLeftChirality(oldConnectedFaces)
    }

    val newFacesSeq = oldFacesSeq
      .updated(relativeIndex(-2), newFaceIMinus2)
      .updated(relativeIndex(-1), newFaceIMinus1)
      .updated(colorIndex, newFaceI)
      .updated(relativeIndex(1), newFaceIPlus1)
      .updated(relativeIndex(2), newFaceIPlus2)

    Cube(
      face0 = newFacesSeq(0),
      face1 = newFacesSeq(1),
      face2 = newFacesSeq(2),
      face3 = newFacesSeq(3),
      face4 = newFacesSeq(4),
      face5 = newFacesSeq(5)
    )
  }

  private def rotateRightChirality(oldConnectedFaces: ConnectedFaces): ConnectedFaces = ConnectedFaces(
    minus2 = oldConnectedFaces.minus2.copy(
      pos6 = oldConnectedFaces.minus1.pos0,
      pos7 = oldConnectedFaces.minus1.pos1,
      pos0 = oldConnectedFaces.minus1.pos2,
    ),
    minus1 = oldConnectedFaces.minus1.copy(
      pos0 = oldConnectedFaces.plus1.pos6,
      pos1 = oldConnectedFaces.plus1.pos7,
      pos2 = oldConnectedFaces.plus1.pos0
    ),
    plus1 = oldConnectedFaces.plus1.copy(
      pos6 = oldConnectedFaces.plus2.pos4,
      pos7 = oldConnectedFaces.plus2.pos5,
      pos0 = oldConnectedFaces.plus2.pos6
    ),
    plus2 = oldConnectedFaces.plus2.copy(
      pos4 = oldConnectedFaces.minus2.pos6,
      pos5 = oldConnectedFaces.minus2.pos7,
      pos6 = oldConnectedFaces.minus2.pos0
    )
  )

  private def rotateLeftChirality(oldConnectedFaces: ConnectedFaces): ConnectedFaces = ConnectedFaces(
    minus2 = oldConnectedFaces.minus2.copy(
      pos2 = oldConnectedFaces.plus2.pos4,
      pos3 = oldConnectedFaces.plus2.pos5,
      pos4 = oldConnectedFaces.plus2.pos6,
    ),
    minus1 = oldConnectedFaces.minus1.copy(
      pos0 = oldConnectedFaces.minus2.pos2,
      pos1 = oldConnectedFaces.minus2.pos3,
      pos2 = oldConnectedFaces.minus2.pos4
    ),
    plus1 = oldConnectedFaces.plus1.copy(
      pos2 = oldConnectedFaces.minus1.pos0,
      pos3 = oldConnectedFaces.minus1.pos1,
      pos4 = oldConnectedFaces.minus1.pos2
    ),
    plus2 = oldConnectedFaces.plus2.copy(
      pos4 = oldConnectedFaces.plus1.pos2,
      pos5 = oldConnectedFaces.plus1.pos3,
      pos6 = oldConnectedFaces.plus1.pos4
    )
  )

  /** Represents the faces connected to a face being rotated
    *
    * The positions are described in terms of how faces are traversed to determine their orientation,
    * but relative to the face being rotated.
    * The two minus faces correspond to the faces prior.
    * The two plus faces correspond to the faces after.
    */
  private case class ConnectedFaces(
    // The face that walks into the face being rotated
    minus2: Face,
    minus1: Face,
    plus1: Face,
    plus2: Face
  )
}
