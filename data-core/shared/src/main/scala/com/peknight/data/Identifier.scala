package com.peknight.data

import cats.{Applicative, Show}
import com.peknight.codec.Codec
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType
import com.peknight.data

trait Identifier:
  def value: String
  override def toString: String = value
end Identifier
object Identifier:
  private case class Identifier(value: String) extends data.Identifier
  def apply(value: String): data.Identifier = Identifier(value)
  given stringCodecIdentifier[F[_]: Applicative]: Codec[F, String, String, data.Identifier] =
    Codec.map[F, String, String, data.Identifier](_.value)(apply)
  given codecIdentifierS[F[_]: Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], data.Identifier] =
    Codec.codecS[F, S, data.Identifier]
end Identifier
