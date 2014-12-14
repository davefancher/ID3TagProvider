﻿/// A series of helper functions intended to make OO ProvidedTypes interface more functional
[<AutoOpen>]
module DidacticCode.ID3.ProvidedTypesHelpers

open System.Collections.Generic
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

let inline makeProvidedConstructor parameters invokeCode =
  ProvidedConstructor(parameters, InvokeCode = invokeCode)

let inline makeProvidedProperty< ^T> propName getterCode =
  ProvidedProperty(propName, typeof< ^T>, GetterCode = getterCode)

let inline makeProvidedMethod< ^T> methodName parameters invokeCode =
  ProvidedMethod(methodName, parameters, typeof< ^T>, InvokeCode = invokeCode)

let inline makeProvidedParameter< ^T> paramName =
  ProvidedParameter(paramName, typeof< ^T>)

let inline addDelayedXmlComment comment providedMember =
  (^a : (member AddXmlDocDelayed : (unit -> string) -> unit) providedMember, (fun () -> comment))
  providedMember
