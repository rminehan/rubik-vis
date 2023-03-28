package rubik

/** Different cube's have different color schemes.
  *
  * Abstractly all that matters is that they are 6 unique colors.
  *
  * The abstract colors are mapped to visual colors at the point of presenting a cube in the UI.
  */
enum Color {
  case Color0, Color1, Color2, Color3, Color4, Color5
}
