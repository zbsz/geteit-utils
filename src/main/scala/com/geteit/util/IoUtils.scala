package com.geteit.util

import java.io._
import java.security.MessageDigest
import java.util.zip.GZIPOutputStream

import scala.util.control.NonFatal

object IoUtils {
  private val buffer = new ThreadLocal[Array[Byte]] {
    override def initialValue(): Array[Byte] = new Array[Byte](8096)
  }

  def copy(in: InputStream, out: OutputStream): Unit = {
    try {
      val buff = buffer.get()
      Iterator continually (in read buff) takeWhile (_ != -1) foreach (size => out.write(buff, 0, size))
      out match {
        case out: FileOutputStream =>
          out.flush()
          out.getFD.sync()
        case _ => // nothing to do
      }
    } finally {
      // make sure both streams are closed
      var rethrow = None: Option[Throwable]
      try { out.close() } catch { case NonFatal(ex) => rethrow = Some(ex) }
      try { in.close() } catch { case NonFatal(ex) => rethrow = Some(ex) }
      rethrow foreach(throw _)
    }
  }

  def copy(in: InputStream, out: File): Unit = {
    out.getParentFile.mkdirs()
    copy(in, new FileOutputStream(out))
  }

  def copy(in: File, out: File): Unit = {
    out.getParentFile.mkdirs()
    copy(new FileInputStream(in), new FileOutputStream(out))
  }

  def toByteArray(in: InputStream) = {
    val out = new ByteArrayOutputStream()
    copy(in, out)
    out.toByteArray
  }

  def gzip(data: Array[Byte]) = {
    val bos = new ByteArrayOutputStream()
    withResource(new GZIPOutputStream(bos)) { os =>
      os.write(data)
      os.finish()
    }
    bos.toByteArray
  }

  def toString(in: InputStream) = new String(toByteArray(in), "utf8")

  def withResource[I <: Closeable, O](in: I)(op: I => O): O = try op(in) finally in.close()

  def md5(file: File): Array[Byte] = withResource(new FileInputStream(file)) { in =>
    val digest = MessageDigest.getInstance("MD5")
    val buff = buffer.get()
    Iterator continually (in read buff) takeWhile (_ != -1) foreach (size => digest.update(buff, 0, size))
    digest.digest()
  }
}
