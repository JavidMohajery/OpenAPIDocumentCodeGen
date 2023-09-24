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

let schema = "https://petstore.swagger.io/v2/swagger.json"
let httpClient = new HttpClient()
let project = "petStore"

let rec createFSharpType (propertyName: string) (required: bool) (propertySchema: OpenApiSchema) =
    if not required
    then 
        let optionalType : SynType = createFSharpType propertyName true propertySchema
        SynType.Option(optionalType)
    else
    match propertySchema.Type with
    | "integer" when propertySchema.Format = "int64" -> SynType.Int64()
    | "integer" when propertySchema.Format = "int32" -> SynType.Int()
    | "string" when propertySchema.Format = "uuid" -> SynType.CreateLongIdent(LongIdentWithDots.Create ["System"; "Guid"])
    | "string" when propertySchema.Format = "date-time" -> SynType.DateTimeOffset()
    | "array" ->
        let arrayItemsType = createFSharpType propertyName required  propertySchema.Items
        SynType.List(arrayItemsType)
    | _ -> SynType.String()

let capitalize (input: string) =
    if not (String.isNotNullOrEmpty input)
    then ""
    else input.First().ToString().ToUpper() + String.Join("", input.Skip(1))

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

    // let enumRepresentation = SynTypeDefnSimpleReprUnionRcd.Create([
    //     for value in values ->
    //         let attrs = [ SynAttributeList.Create [|compiledName value|] ]
    //         let docs = PreXmlDoc.Empty
    //         SynUnionCase.SynUnionCase(attrs, Ident.Create (capitalize value), SynUnionCaseType.UnionCaseFields [], docs, None, Range.range0, { BarRange = None})
    // ])

    let simpleType = SynTypeDefnSimpleReprRcd.Union(enumRepresentation)
    SynModuleDecl.CreateSimpleType(info, simpleType)

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

        let recordRepresentation = SynTypeDefnSimpleReprRecordRcd.Create [
            for property in schema.Properties do
                let propertyName = property.Key
                let propertyType = property.Value
                let field = SynFieldRcd.Create(propertyName, createFSharpType propertyName (schema.Required.Contains propertyName) propertyType)
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
            createRecordFromSchema schema.Key schema.Value
    ]
    let globalTypesModule = CodeGen.createQualifiedModule [project;"Types"] globalTypes
    let code = CodeGen.formatAst (CodeGen.createFile "Test" [globalTypesModule]) 
    System.Console.WriteLine code

    let message = "From F#"
    printfn $"Hello world from {message}"
    0