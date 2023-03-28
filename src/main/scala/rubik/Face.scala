package rubik

/** Represents a face on a rubik's cube.
  *
  * The positions on the cube move clockwise around the cube starting at the top left.
  *
  * This presupposes a viewing orientation which is explained on the cube itself.
  *
  * Note that the middle square of the face is not represented as it's effectively pinned to the face.
  */
case class Face(
  // Top left
  pos0: Color,
  // Top middle
  pos1: Color,
  // Top right
  pos2: Color,
  // Middle right
  pos3: Color,
  // Bottom right
  pos4: Color,
  // Bottom middle
  pos5: Color,
  // Bottom left
  pos6: Color,
  // Middle left
  pos7: Color
)
