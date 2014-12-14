﻿namespace DidacticCode.ID3

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

// Disable incomplete matches warning
// Incomplete matches are used extensively within this file
// to simplify the code
#nowarn "0025"

[<TypeProvider>]
type ID3TagProvider() as this =
  inherit TypeProviderForNamespaces()
  let ns = "DidacticCode.TypeProviders"
  let assy = Assembly.GetExecutingAssembly()

  let taggedFileType = ProvidedTypeDefinition(assy, ns, "AudioFile", None)

  let buildExpr tag =
    fun [tags] -> <@@ (((%%tags:obj) :?> Dictionary<string, ID3Frame>).[tag]).GetContent() |> unbox @@>

  do
    taggedFileType.DefineStaticParameters(
      [ ProvidedStaticParameter("fileName", typeof<string>) ],
      instantiationFunction = (
        fun typeName [| :? string as fileName |] -> 
          let ty = ProvidedTypeDefinition(assy, ns, typeName, None)

          makeProvidedConstructor
              [ ]
              (fun [] -> <@@ fileName |> ID3Reader.readID3Tags @@>)
          |> ty.AddMember

          makeProvidedMethod<bool>
            "HasTag"
            ([ ProvidedParameter("tag", typeof<string>) ])
            (fun [ tags; tag ] -> <@@ ((%%tags:obj) :?> Dictionary<string, ID3Frame>).ContainsKey(%%tag:string) @@>)
          |> addDelayedXmlComment "Returns a value indicating whether the specified tag was located within the source file"
          |> ty.AddMember

          makeProvidedMethod<ID3Frame option>
            "GetTag"
            [ ProvidedParameter("tag", typeof<string>)]
            (fun [ tags; tag ] -> <@@ let tagDict = ((%%tags:obj) :?> Dictionary<string, ID3Frame>)
                                      if tagDict.ContainsKey(%%tag:string) then Some tagDict.[(%%tag:string)]
                                      else None @@>)
          |> addDelayedXmlComment "Returns an ID3Frame object representing the specific tag"
          |> ty.AddMember

          fileName
          |> ID3Reader.readID3Tags
          |> Seq.map (fun i -> match i.Key with
                                | "APIC" as tag ->
                                    makeProvidedProperty<AttachedPicture> "AttachedPicture" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the album art attached to the file. Corresponds to the APIC tag."
                                | "MCDI" as tag ->
                                    makeProvidedProperty<string> "CdIdentifier" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the CD Identifier. Corresponds to the MCDI tag."
                                | "POPM" as tag ->
                                    makeProvidedProperty<Popularimeter> "Popularimeter" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the Popularimeter data including play count and rating. Corresponds to the POPM tag."
                                | "TALB" as tag ->
                                    makeProvidedProperty<string> "AlbumTitle" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the album title. Corresponds to the TALB tag."
                                | "TIT1" as tag ->
                                    makeProvidedProperty<string> "ContentGroup" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the content group. Corresponds to the TIT1 tag."
                                | "TIT2" as tag ->
                                    makeProvidedProperty<string> "TrackTitle" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track title. Corresponds to the TIT2 tag."
                                | "TIT3" as tag ->
                                    makeProvidedProperty<string> "TrackSubtitle" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track subtitle. Corresponds to the TIT3 tag."
                                | "TRCK" as tag ->
                                    makeProvidedProperty<string> "TrackNumber" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track number. Corresponds to the TRCK tag."
                                | "TYER" as tag ->
                                    makeProvidedProperty<string> "Year" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the year the track was released. Corresponds to the TYER tag."
                                | "TPE1" as tag ->
                                    makeProvidedProperty<string> "Performer" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track performer's name. Corresponds to the TPE1 tag."
                                | "TPE2" as tag ->
                                    makeProvidedProperty<string> "Band" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the band name. Corresponds to the TPE2 tag."
                                | "TPOS" as tag ->
                                    makeProvidedProperty<string> "SetIdentifier" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track's position within the set. Corresponds to the TPOS tag."
                                | "TPUB" as tag ->
                                    makeProvidedProperty<string> "Publisher" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track publisher's name. Corresponds to the TPUB tag."
                                | "TCOM" as tag ->
                                    makeProvidedProperty<string> "Composer" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track composer's name. Corresponds to the TCOM tag."
                                | "TCON" as tag ->
                                    makeProvidedProperty<string> "ContentType" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track's content type. Corresponds to the TCON tag."
                                | "TCOP" as tag ->
                                    makeProvidedProperty<string> "Copyright" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the copyright information for the track. Corresponds to the TCOP tag."
                                | "TLEN" as tag ->
                                    makeProvidedProperty<string> "TrackLength" tag (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the length of the track. Corresponds to the TLEN tag.")
          |> Seq.toList
          |> ty.AddMembers

          ty
      ))

  do
    this.AddNamespace(ns, [ taggedFileType ])

[<assembly:TypeProviderAssembly>]
do ()