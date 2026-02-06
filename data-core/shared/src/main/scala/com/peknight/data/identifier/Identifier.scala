package com.peknight.data.identifier

import cats.{Applicative, Show}
import com.peknight.codec.Codec
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType

trait Identifier:
  def value: String
  override def toString: String = value
end Identifier
object Identifier:
  private case class Identifier(value: String) extends com.peknight.data.identifier.Identifier
  def apply(value: String): com.peknight.data.identifier.Identifier = Identifier(value)
  given stringCodecIdentifier[F[_]: Applicative]: Codec[F, String, String, com.peknight.data.identifier.Identifier] =
    Codec.map[F, String, String, com.peknight.data.identifier.Identifier](_.value)(apply)
  given codecIdentifierS[F[_]: Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], com.peknight.data.identifier.Identifier] =
    Codec.codecS[F, S, com.peknight.data.identifier.Identifier]
end Identifier
