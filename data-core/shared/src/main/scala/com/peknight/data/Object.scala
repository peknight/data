package com.peknight.data

import cats.data.Kleisli
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*
import cats.syntax.show.*
import cats.{Applicative, Eq, Foldable, Monad, Semigroup, Show}

trait Object[K, V] extends Serializable:
  def applyUnsafe(key: K): V = apply(key).getOrElse(throw new NoSuchElementException(s"key not found: $key"))
  def apply(key: K): Option[V]
  def contains(key: K): Boolean
  def size: Int
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
  def kleisli: Kleisli[Option, K, V] = Kleisli(apply)
  def keys: Iterable[K]
  def values: Iterable[V]
  def toMap: Map[K, V]
  def toIterable: Iterable[(K, V)]
  def toList: List[(K, V)] = toIterable.toList
  def toVector: Vector[(K, V)] = toIterable.toVector
  def add(key: K, value: V): Object[K, V]
  def prepended(field: (K, V)): Object[K, V]
  def +:(field: (K, V)): Object[K, V] = prepended(field)
  def remove(key: K)(using Eq[K]): Object[K, V]
  def remove(keys: Seq[K])(using Eq[K]): Object[K, V]
  def traverseValues[F[_], T](f: V => F[T])(using Applicative[F]): F[Object[K, T]]
  def mapValues[T](f: V => T): Object[K, T]
  def filter(pred: ((K, V)) => Boolean): Object[K, V] = Object.fromIterable[K, V](toIterable.filter(pred))
  def filterKeys(pred: K => Boolean): Object[K, V] = filter(field => pred(field._1))
  def deepMerge(that: Object[K, V])(using Semigroup[V]): Object[K, V] =
    toIterable.foldLeft(that) {
      case (acc, (key, value)) =>
        that(key).fold(acc.add(key, value)) { r =>
          acc.add(key, value |+| r)
        }
    }
  def foldLeft[B](b: B)(f: (B, (K, V)) => B): B
  def foldLeftM[F[_]: Monad, B](b: B)(f: (B, (K, V)) => F[B]): F[B]
  override def toString: String = toIterable.map((key, value) => s"$key=$value").mkString("Object(", ",", ")")
end Object
object Object:
  def apply[K, V](fields: (K, V)*): Object[K, V] = fromIterable(fields)
  def fromFoldable[F[_], K, V](fields: F[(K, V)])(using Foldable[F]): Object[K, V] =
    fields.foldLeft(empty) {
      case (acc, (key, value)) => acc.add(key, value)
    }
  def fromIterable[K, V](fields: Iterable[(K, V)]): Object[K, V] =
    MapAndVectorObject[K, V](fields.toMap, fields.map(_._1).toVector)
  def fromMap[K, V](map: Map[K, V]): Object[K, V] = fromMapAndVector(map, map.keys.toVector)
  private def fromMapAndVector[K, V](map: Map[K, V], keys: Vector[K]): Object[K, V] =
    MapAndVectorObject[K, V](map, keys)
  def empty[K, V]: Object[K, V] = MapAndVectorObject[K, V](Map.empty, Vector.empty)
  def singleton[K, V](key: K, value: V): Object[K, V] = MapAndVectorObject[K, V](Map((key, value)), Vector(key))
  private class MapAndVectorObject[K, V](fields: Map[K, V], orderedKeys: Vector[K]) extends Object[K, V]:
    override def applyUnsafe(key: K): V = fields(key)
    def apply(key: K): Option[V] = fields.get(key)
    def contains(key: K): Boolean = fields.contains(key)
    def size: Int = fields.size
    def isEmpty: Boolean = fields.isEmpty
    def keys: Iterable[K] = orderedKeys
    def values: Iterable[V] = orderedKeys.map(key => fields(key))
    def toMap: Map[K, V] = fields
    def toIterable: Iterable[(K, V)] = orderedKeys.map(key => (key, fields(key)))
    def add(key: K, value: V): Object[K, V] =
      if fields.contains(key) then MapAndVectorObject[K, V](fields.updated(key, value), orderedKeys)
      else MapAndVectorObject[K, V](fields.updated(key, value), orderedKeys :+ key)
    def prepended(field: (K, V)): Object[K, V] =
      val (key, value) = field
      if fields.contains(key) then MapAndVectorObject[K, V](fields.updated(key, value), orderedKeys)
      else MapAndVectorObject[K, V](fields.updated(key, value), key +: orderedKeys)
    def remove(key: K)(using Eq[K]): Object[K, V] = MapAndVectorObject[K, V](fields - key, orderedKeys.filterNot(_ === key))
    def remove(keys: Seq[K])(using Eq[K]): Object[K, V] = MapAndVectorObject[K, V](fields -- keys, orderedKeys.filterNot(k => keys.exists(_ === k)))
    def traverseValues[F[_], T](f: V => F[T])(using Applicative[F]): F[Object[K, T]] =
      orderedKeys.foldLeft(Map.empty[K, T].pure[F]) {
        case (acc, key) => (acc, f(fields(key))).mapN(_.updated(key, _))
      }.map(mappedFields => MapAndVectorObject[K, T](mappedFields, orderedKeys))
    def mapValues[T](f: V => T): Object[K, T] =
      MapAndVectorObject[K, T](
        fields.map {
          case (key, value) => (key, f(value))
        },
        orderedKeys
      )
    def foldLeft[B](b: B)(f: (B, (K, V)) => B): B =
      orderedKeys.foldLeft[B](b)((z, key) => f(z, (key, fields(key))))

    def foldLeftM[G[_]: Monad, B](b: B)(f: (B, (K, V)) => G[B]): G[B] =
      orderedKeys.foldLeftM[G, B](b)((z, key) => f(z, (key, fields(key))))
  end MapAndVectorObject
  given showObject[K, V](using Show[K], Show[V]): Show[Object[K, V]] with
    def show(t: Object[K, V]): String =
      t.toIterable.map((key, value) => s"${key.show}=${value.show}").mkString("Object(", ",", ")")
  end showObject
end Object
