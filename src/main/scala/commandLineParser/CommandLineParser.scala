package commandLineParser

import scopt.OParser


object CommandLineParser {

  // TODO: version etc
  private val builder = OParser.builder[ParserConfig]
  private val parser = {
    import builder._
    OParser.sequence(
      programName("[Program name]"),
      head("ABA-DD: Rule based.", "0.3"),
      // option -f, --file
      // TODO: File instead of string?
      arg[String]("<input file>")
        .required()
        //.valueName("<input file>")
        .action((x, c) => c.copy(inputFilePath = x)),
      //.text("path to input file"),
      opt[String]('i', "inputFormat")
        // TODO:
        .action((x, c) => c.copy(inputFormat = x))
        .validate {
          case "aba" | "apx" => success
          case _ => failure("Input format must be one of: aba or apx ")
        }
        .text("Input format. Possible values are aba (default) and apx")
        .valueName("<input format>")
    )
  }

  def parse(args: Array[String]): Option[ParserConfig] = OParser.parse(parser, args, ParserConfig())

}
