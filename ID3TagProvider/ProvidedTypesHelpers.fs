/// A series of helper functions intended to make OO ProvidedTypes interface more functional
[<AutoOpen>]
module DidacticCode.ID3.ProvidedTypesHelpers

open System.Collections.Generic
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

let inline makeProvidedConstructor parameters invokeCode =
  ProvidedConstructor(parameters, InvokeCode = invokeCode)

let inline makeReadOnlyProvidedProperty< ^T> getterCode propName =
  ProvidedProperty(propName, typeof< ^T>, GetterCode = getterCode)

let inline makeProvidedMethod< 'T> parameters invokeCode methodName =
  ProvidedMethod(methodName, parameters, typeof< 'T>, InvokeCode = invokeCode)

let inline makeProvidedParameter< ^T> paramName =
  ProvidedParameter(paramName, typeof< ^T>)

let inline addDelayedXmlComment comment providedMember =
  (^a : (member AddXmlDocDelayed : (unit -> string) -> unit) providedMember, (fun () -> comment))
  providedMember

let inline makeTagPropertyWithComment tag comment =
  let expr =
    fun [tags] ->
      <@@ (((%%tags:obj) :?> Dictionary<string, ID3Frame>).[tag]).GetContent() |> unbox @@>
  (makeReadOnlyProvidedProperty<string> expr)>> (addDelayedXmlComment comment)