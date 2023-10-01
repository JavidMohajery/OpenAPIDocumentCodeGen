open Microsoft.OpenApi.Readers
open System.Net.Http
open FsAst
open Fantomas
open FSharp.Compiler.SyntaxTree
open Microsoft.OpenApi.Models
open FSharp.Compiler.XmlDoc
open FSharp.Compiler.Range
open System
open System.Linq
open System.IO

let resolvePath (path:string)  =
    if Path.IsPathRooted path
    then path
    else Path.GetFullPath (Path.Combine(Environment.CurrentDirectory, path))

let httpClient = new HttpClient()
let getSchema (schemaPath: string) =
    if File.Exists schemaPath
    then new FileStream(schemaPath, FileMode.Open) :> Stream
    elif schemaPath.StartsWith "http"
    then 
        httpClient.GetStreamAsync(schemaPath)
            |> Async.AwaitTask
            |> Async.RunSynchronously
    else
        failwith "failed to create a stream for the specified schema path"
let schemaPath = "./petstore.json"
let schema = "https://petstore.swagger.io/v2/swagger.json"
let project = "petStore"

let capitalize (input: string) =
    if not (String.isNotNullOrEmpty input)
    then ""
    else input.First().ToString().ToUpper() + String.Join("", input.Skip(1))

let rec createFieldType (recordName: string) (propertyName: string) (required: bool) (propertySchema: OpenApiSchema) =
    if not required
    then 
        let optionalType : SynType = createFieldType recordName propertyName true propertySchema
        SynType.Option(optionalType)
    else
    match propertySchema.Type with
    | "integer" when propertySchema.Format = "int64" -> SynType.Int64()
    | "integer" -> SynType.Int()
    | "number" when propertySchema.Format = "float" -> SynType.Create "float32"
    | "number" -> SynType.Create "double"
    | "string" when propertySchema.Format = "uuid" -> SynType.CreateLongIdent(LongIdentWithDots.Create ["System"; "Guid"])
    | "string" when propertySchema.Format = "date-time" -> SynType.DateTimeOffset()
    | "string" when propertySchema.Format = "byte" -> SynType.Array(1, SynType.Create "buyte", range0)
    | "array" ->
        let arrayItemsType = createFieldType recordName propertyName required  propertySchema.Items
        SynType.List(arrayItemsType)
    | "string" when not (isNull propertySchema.Enum) && propertySchema.Enum.Count > 0 ->
        SynType.Create (recordName + capitalize propertyName)
    | _ when not (isNull propertySchema.Reference) -> SynType.Create propertySchema.Reference.Id
    | _ -> SynType.String()


let compiledName (name: string) = SynAttribute.Create("CompiledName", name)
let createEnumType (enumType: (string * seq<string>)) =
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [
            SynAttributeList.Create [
                SynAttribute.Create ""
                SynAttribute.RequireQualifiedAccess()
            ]
        ]

        Id = [ Ident.Create (fst enumType) ]
        XmlDoc = PreXmlDoc.Empty
        Parameters = [ ]
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let values = snd enumType

    let enumRepresentation = SynTypeDefnSimpleReprUnionRcd.Create [
        for value in values do
            let attrs = [ SynAttributeList.Create [|compiledName value|]]
            SynUnionCase.UnionCase (attrs, Ident.Create (capitalize value), SynUnionCaseType.UnionCaseFields [], PreXmlDoc.Empty, None, range0)
    ]

    let simpleType = SynTypeDefnSimpleReprRcd.Union(enumRepresentation)


    let members: SynMemberDefn list = [
        let constRecord : SynPatConstRcd = {
            Const = SynConst.Unit
            Range = range0
        }
        let clauses : SynMatchClause list = [
            for value in values do
                let id = LongIdentWithDots.CreateString(capitalize value)
                let pattern = SynPat.LongIdent(id, None, None, SynArgPats.Empty, None, range0)
                SynMatchClause.Clause(pattern, None, SynExpr.Const(SynConst.CreateString(value), range0), range0, DebugPointForTarget.No)
        ]
        SynMemberDefn.CreateMember 
            { SynBindingRcd.Null with 
                Pattern = SynPatRcd.CreateLongIdent (LongIdentWithDots.CreateString("this.Format"), [SynPatRcd.Const constRecord])
                Expr = SynExpr.Match(DebugPointForBinding.DebugPointAtBinding range0, SynExpr.Ident(Ident.Create("this")), clauses, range0)
            }
    ]
    SynModuleDecl.CreateSimpleType(info, simpleType, members)

[<EntryPoint>]
let main argv =
    let schema = getSchema (resolvePath "./petStore.json")
    let reader = new OpenApiStreamReader()
    let (openApiDocument, diagnostic: OpenApiDiagnostic) = reader.Read(schema)

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

        let recordRepresentation = SynTypeDefnSimpleReprRecordRcd.Create [
            for property in schema.Properties do
                let propertyName = property.Key
                let propertyType = property.Value
                let field = SynFieldRcd.Create(propertyName, createFieldType recordName propertyName (schema.Required.Contains propertyName) propertyType)
                let docs = PreXmlDoc.Create [if String.isNotNullOrEmpty propertyType.Description then propertyType.Description]
                {field with XmlDoc = docs}
        ]
        let simpleType= SynTypeDefnSimpleReprRcd.Record recordRepresentation
        SynModuleDecl.CreateSimpleType(info, simpleType)

    
    let rec findEnumTypes (parentName: string) (enumName: string Option) (schema: OpenApiSchema) =
        if not (isNull schema.Enum) && schema.Enum.Count > 0 then
            match enumName with
            | Some name -> 
                let enumCases = 
                    schema.Enum
                    |> Seq.choose(fun enumCase -> 
                        match enumCase with
                        | :? Microsoft.OpenApi.Any.OpenApiString as primitivalue -> Some primitivalue.Value
                        | _ -> None
                        )
                [(name, enumCases)]
            | None -> []
        else
            [
                for property in schema.Properties do
                    let propertyName = property.Key
                    let propertySchema = property.Value
                    yield! findEnumTypes parentName (Some (parentName + capitalize propertyName)) propertySchema
            ]


    let enumDefinitions = [
        for schema in openApiDocument.Components.Schemas do
            let typeName = schema.Key
            for (enumName, enumCases) in findEnumTypes typeName None schema.Value do
                if not (Seq.isEmpty enumCases) then
                    enumName, enumCases
    ]

    let enumTypes = 
        enumDefinitions
        |> List.map createEnumType

    let globalTypes = [
        yield! enumTypes
        for schema in openApiDocument.Components.Schemas do
            let typeName = 
                if String.IsNullOrEmpty schema.Value.Title
                then schema.Key
                else schema.Value.Title  
            createRecordFromSchema typeName schema.Value
    ]
    let globalTypesModule = CodeGen.createQualifiedModule [project;"Types"] globalTypes
    let code = CodeGen.formatAst (CodeGen.createFile "Test" [globalTypesModule]) 
    System.Console.WriteLine code

    let message = "From F#"
    printfn $"Hello world from {message}"
    0