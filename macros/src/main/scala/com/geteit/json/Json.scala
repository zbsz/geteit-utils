package com.geteit.json

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox

class Json extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Json.jsonMacroImpl.impl
}

class JsonValue extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Json.jsonValueMacroImpl.impl
}

object Json {
  object jsonMacroImpl extends JsonMacro
  object jsonValueMacroImpl extends JsonMacro {
    override protected def jsonCodecs(c: whitebox.Context)(className: c.universe.TypeName, fields: List[c.universe.ValDef]): Seq[c.Tree] = {
      if (fields.length != 1) c.abort(c.enclosingPosition, s"Cannot create json value codecs for case class with ${fields.length} fields")
      import c.universe._
      Seq (
        q"implicit val jsonEncoder = com.geteit.json.JsonEncoder.valueEncoder[$className]",
        q"implicit val jsonDecoder = com.geteit.json.JsonDecoder.valueDecoder[$className]"
      )
    }
  }
}

trait JsonMacro {

  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def extractClassNameAndFields(classDecl: ClassDef): (TypeName, List[ValDef]) = {
      try {
        val q"case class $className(..$fields) extends ..$bases { ..$body }" = classDecl
        (className, fields)
      } catch {
        case _: MatchError => c.abort(c.enclosingPosition, "Annotation is only supported on case class")
      }
    }

    def modifiedCompanion(compDeclOpt: Option[ModuleDef], codecs: Seq[Tree], className: TypeName) = {
      compDeclOpt map { compDecl =>
        // Add the formatter to the existing companion object
        val q"object $obj extends ..$bases { ..$body }" = compDecl
        q"""
          object $obj extends ..$bases {
            ..$codecs
            ..$body
          }
        """
      } getOrElse {
        // Create a companion object with the formatter
        q"object ${className.toTermName} { ..$codecs }"
      }
    }

    def modifiedDeclaration(classDecl: ClassDef, compDeclOpt: Option[ModuleDef] = None) = {
      val (className, fields) = extractClassNameAndFields(classDecl)
      val codecs = jsonCodecs(c)(className, fields)
      val compDecl = modifiedCompanion(compDeclOpt, codecs, className)

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

  protected def jsonCodecs(c: whitebox.Context)(className: c.universe.TypeName, fields: List[c.universe.ValDef]): Seq[c.Tree] = {
    import c.universe._
    Seq (
      q"implicit val jsonEncoder = com.geteit.json.JsonEncoder[$className]",
      q"implicit val jsonDecoder = com.geteit.json.JsonDecoder[$className]"
    )
  }
}
