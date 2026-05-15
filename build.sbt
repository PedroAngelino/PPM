name := "ProjetoKonane"

version := "1.0"

scalaVersion := "3.3.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Sistema operativo desconhecido!")
}

// Módulos do JavaFX que o teu código precisa (base, controlos de botões e gráficos 2D)
lazy val javaFXModules = Seq("base", "controls", "graphics")

libraryDependencies ++= javaFXModules.map(m =>
  "org.openjfx" % s"javafx-$m" % "21.0.2" classifier osName
)