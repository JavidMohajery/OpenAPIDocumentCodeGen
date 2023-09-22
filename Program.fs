open Microsoft.OpenApi.Readers
open System.Net.Http
open FsAst
open Fantomas
open FSharp.Compiler.SyntaxTree
open Microsoft.OpenApi.Models
open FSharp.Compiler.XmlDoc
open FSharp.Compiler.Range
let schema = "https://petstore.swagger.io/v2/swagger.json"
let httpClient = new HttpClient()
let project = "petStore"
[<AutoOpen>]
module Extensions =
    type SynFieldRcd with
        static member Create(name: string, fieldType: SynType) =
            {
                Access = None
                Attributes = []
                Id = Some (Ident.Create name)
                IsMutable = false
                IsStatic = false
                Range = range0
                Type = fieldType
                XmlDoc = PreXmlDoc.Empty
            }

        static member Create(name: string, fieldType: string) =
            {
                Access = None
                Attributes = []
                Id = Some (Ident.Create name)
                IsMutable = false
                IsStatic = false
                Range = range0
                Type = SynType.Create fieldType
                XmlDoc = PreXmlDoc.Empty
            }
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

[<EntryPoint>]
let main argv =
    let response = 
        httpClient.GetStreamAsync(schema)
        |> Async.AwaitTask
        |> Async.RunSynchronously
    let reader = new OpenApiStreamReader()
    let (openApiDocument, diagnostic: OpenApiDiagnostic) = reader.Read(response)

    let createRecordFromSchema (recordName: string) (schema : OpenApiSchema) =
        let info: SynComponentInfoRcd = {
            Access = None
            Attributes = []
            Id = [ Ident.Create recordName]
            XmlDoc = PreXmlDoc.Create [if String.isNotNullOrEmpty schema.Description then schema.Description]
            Parameters = []
            Constraints = []
            PreferPostfix = false
            Range = range0
        }
        let rec createFSharpType (propertyName: string) (required: bool) (schema: OpenApiSchema) =
            match schema.Type with
            | "integer" when schema.Format = "int64" -> 
                if required
                then SynType.Int64()
                else SynType.Option(SynType.Int64())
            | "integer" when schema.Format = "int32" ->  
                if required
                then SynType.Int()
                else SynType.Option(SynType.Int())
            | "array" ->
                let arrayItemsType = createFSharpType propertyName required  schema.Items
                if required
                then SynType.List(arrayItemsType)
                else SynType.Option(SynType.List(arrayItemsType))
            | _ -> SynType.String()

        let recordRepresentation = SynTypeDefnSimpleReprRecordRcd.Create [
            for property in schema.Properties do
                let propertyName = property.Key
                let propertyType = property.Value
                SynFieldRcd.Create(propertyName, createFSharpType propertyName (schema.Required.Contains propertyName) propertyType)
        ]
        let simpleType= SynTypeDefnSimpleReprRcd.Record recordRepresentation
        SynModuleDecl.CreateSimpleType(info, simpleType)


    let globalTypes = [
        for schema in openApiDocument.Components.Schemas do
            createRecordFromSchema schema.Key schema.Value
    ]
    let globalTypesModule = createQualifiedModule [project;"Types"] globalTypes
    let code = formatAst (createFile "Test" [globalTypesModule]) 
    System.Console.WriteLine code

    let message = "From F#"
    printfn $"Hello world from {message}"
    0