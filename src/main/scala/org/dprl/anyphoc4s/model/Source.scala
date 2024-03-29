package org.dprl.anyphoc4s.model


sealed trait Source

class Geo2D extends Source
class TreeBased extends Source

case class SVG(contents: String) extends Geo2D
