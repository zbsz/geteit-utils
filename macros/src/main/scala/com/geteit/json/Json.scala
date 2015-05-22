package com.geteit.json

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox

class Json extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Json.impl
}

object Json {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def extractClassName(classDecl: ClassDef): TypeName = {
      try {
        val q"case class $className(..$fields) extends ..$bases { ..$body }" = classDecl
        className
      } catch {
        case _: MatchError => c.abort(c.enclosingPosition, "Annotation is only supported on case class")
      }
    }

    def jsonFormatter(className: TypeName): Seq[Tree] = Seq (
      q"implicit val jsonEncoder = com.geteit.json.JsonEncoder[$className]",
      q"implicit val jsonDecoder = com.geteit.json.JsonDecoder[$className]"
    )

    def modifiedCompanion(compDeclOpt: Option[ModuleDef], format: Seq[Tree], className: TypeName) = {
      compDeclOpt map { compDecl =>
        // Add the formatter to the existing companion object
        val q"object $obj extends ..$bases { ..$body }" = compDecl
        q"""
          object $obj extends ..$bases {
            ..$body
            ..$format
          }
        """
      } getOrElse {
        // Create a companion object with the formatter
        q"object ${className.toTermName} { ..$format }"
      }
    }

    def modifiedDeclaration(classDecl: ClassDef, compDeclOpt: Option[ModuleDef] = None) = {
      val className = extractClassName(classDecl)
      val format = jsonFormatter(className)
      val compDecl = modifiedCompanion(compDeclOpt, format, className)

      // Return both the class and companion object declarations
      c.Expr(q"""
        $classDecl
        $compDecl
      """)
    }

    annottees.map(_.tree) match {
      case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)
      case (classDecl: ClassDef) :: (compDecl: ModuleDef) :: Nil => modifiedDeclaration(classDecl, Some(compDecl))
      case _ => c.abort(c.enclosingPosition, "Invalid annottee")
    }
  }
}
