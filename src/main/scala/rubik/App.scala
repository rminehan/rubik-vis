package rubik

import rubik.Color._

@main
def app(): Unit = {
  val start = Cube.solved

  val colorScheme = ColorScheme(
    color0 = 'G', // green
    color1 = 'R', // red
    color2 = 'W', // white
    color3 = 'B', // blue
    color4 = 'O', // orange
    color5 = 'Y'  // yellow
  )

  println("Starting out with a solved cube")
  println(render(start, colorScheme))

  val rotated = Cube.rotateClockwise(start, Color0)

  println("Rotated 90 degrees clockwise around 'G' (green)")
  println(render(rotated, colorScheme))
}

/** Maps the colors to chars
  *
  * e.g. color0 = 'G' would represent how it's green.
  *
  * TODO - make the color scheme represent graphical colors (e.g. a java.awt.Color)
  * or a color from jline which can be rendered colorfully to a terminal.
  */
private case class ColorScheme(
  color0: Char,
  color1: Char,
  color2: Char,
  color3: Char,
  color4: Char,
  color5: Char
)

/** Renders a cube to text intended for display on a cli.
  *
  * The faces are drawn with face 0 at the top and face 5 at the bottom.
  */
private def render(cube: Cube, colorScheme: ColorScheme): String = {
  def renderFace(face: Face, faceColor: Color): String = {
    def col(color: Color): Char = color match {
      case Color0 => colorScheme.color0
      case Color1 => colorScheme.color1
      case Color2 => colorScheme.color2
      case Color3 => colorScheme.color3
      case Color4 => colorScheme.color4
      case Color5 => colorScheme.color5

    }
    s"""| -------------
        ||${col(face.pos0)}     ${col(face.pos1)}     ${col(face.pos2)}|
        ||             |
        ||${col(face.pos7)}     ${col(faceColor)}     ${col(face.pos3)}|
        ||             |
        ||${col(face.pos6)}     ${col(face.pos5)}     ${col(face.pos4)}|
        | -------------
       """.stripMargin
  }

  Seq(
    (cube.face0, Color0),
    (cube.face1, Color1),
    (cube.face2, Color2),
    (cube.face3, Color3),
    (cube.face4, Color4),
    (cube.face5, Color5)
  ).map((face, faceColor) => renderFace(face, faceColor)).mkString("\n")
}
