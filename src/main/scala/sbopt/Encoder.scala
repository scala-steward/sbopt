package sbopt

import java.io.File
import java.nio.file.Path

trait Encoder[A] {
  def encode(a: A): String
}

object Encoder {
  implicit val stringEncoder: Encoder[String] = new Encoder[String] {
    override def encode(a: String): String = a
  }
  implicit val intEncoder: Encoder[Int] = new Encoder[Int] {
    override def encode(a: Int): String = String.valueOf(a)
  }
  implicit val longEncoder: Encoder[Long] = new Encoder[Long] {
    override def encode(a: Long): String = String.valueOf(a)
  }
  implicit val floatEncoder: Encoder[Float] = new Encoder[Float] {
    override def encode(a: Float): String = String.valueOf(a)
  }
  implicit val doubleEncoder: Encoder[Double] = new Encoder[Double] {
    override def encode(a: Double): String = String.valueOf(a)
  }
  implicit val booleanEncoder: Encoder[Boolean] = new Encoder[Boolean] {
    override def encode(a: Boolean): String = String.valueOf(a)
  }
  implicit val fileEncoder: Encoder[File] = new Encoder[File] {
    override def encode(a: File): String = a.getAbsolutePath()
  }
  implicit val pathEncoder: Encoder[Path] = new Encoder[Path] {
    override def encode(a: Path): String = a.toAbsolutePath().toString
  }
  implicit def tuple2Encoder[A: Encoder, B: Encoder]: Encoder[(A, B)] =
    new Encoder[Tuple2[A, B]] {
      override def encode(a: (A, B)): String = {
        val encoderA = implicitly[Encoder[A]]
        val encoderB = implicitly[Encoder[B]]
        s"${encoderA.encode(a._1)}=${encoderB.encode(a._2)}"
      }
    }
  implicit def optionEncoder[A: Encoder]: Encoder[Option[A]] =
    new Encoder[Option[A]] {
      override def encode(a: Option[A]): String = {
        val encoderA = implicitly[Encoder[A]]
        a.fold("")(encoderA.encode)
      }
    }
  implicit def someEncoder[A: Encoder]: Encoder[Some[A]] =
    new Encoder[Some[A]] {
      override def encode(a: Some[A]): String = {
        val encoderA = implicitly[Encoder[A]]
        encoderA.encode(a.get)
      }
    }
  implicit def noneEncoder: Encoder[None.type] =
    new Encoder[None.type] {
      override def encode(a: None.type): String = ""
    }
  implicit def listEncoder[A: Encoder]: Encoder[List[A]] =
    new Encoder[List[A]] {
      override def encode(a: List[A]): String = {
        val encoder = implicitly[Encoder[A]]
        a.map(encoder.encode).mkString(",")
      }
    }
  implicit def from[A, B: Encoder](f: A => B): Encoder[A] =
    new Encoder[A] {
      override def encode(a: A): String = {
        val encoder = implicitly[Encoder[B]]
        encoder.encode(f(a))
      }
    }
}
