[<AutoOpen>]
module CodeGen
open FSharp.Compiler.XmlDoc
open FSharp.Compiler.SyntaxTree
open Fantomas
open FsAst
open FSharp.Compiler.Range

let createQualifiedModule (idens: seq<string>) declarations =
    let nameParts = 
        idens
        |> Seq.collect(fun name ->
            if name .Contains "."
            then name.Split(".")
            else [|name|]
        )
    let xmlDoc = PreXmlDoc.Create []
    SynModuleOrNamespace([for idents in nameParts -> Ident.Create idents], true, SynModuleOrNamespaceKind.NamedModule, declarations, PreXmlDoc.Create [], [SynAttributeList.Create [SynAttribute.RequireQualifiedAccess()]], None, range0)

let createFile fileName modules =
    let qualifiedNameOfFile = QualifiedNameOfFile.QualifiedNameOfFile(Ident.Create fileName)
    ParsedImplFileInput.ParsedImplFileInput(fileName, false, qualifiedNameOfFile, [], [], modules, (false, false))

let formatAstInternal ast =
    let cng = {FormatConfig.FormatConfig.Default with StrictMode = true}
    CodeFormatter.FormatASTAsync(ast, "temp.fx", [], None, cng)
let formatAst file =
    formatAstInternal (ParsedInput.ImplFile file)
    |> Async.RunSynchronously