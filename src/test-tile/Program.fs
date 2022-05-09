open System
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open FSharp.Data.Adaptive
open System.Collections.Generic

module Shader =
    open FShade

    type UniformScope with  
        member x.OffsetAndScale : V4d = uniform?OffsetAndScale

    let sammy =
        sampler2d {
            texture uniform?DiffuseColorTexture
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.Anisotropic
            maxAnisotropy 16
        }

    let tex (v : Effects.Vertex) =
        fragment {
            let tc = v.tc * uniform.OffsetAndScale.ZW + uniform.OffsetAndScale.XY
            return sammy.Sample(tc)
        }

[<EntryPoint;STAThread>]
let main argv = 
    Aardvark.Init()

    use app = new OpenGlApplication()
    use win = app.CreateGameWindow(4)
    //let s = V2i(38768,128)
    let pos = [| V3f.OOO; V3f.IOO; V3f.IIO; V3f.OIO|]
    
    // test image: https://i.nahraj.to/f/1Nkf.jpg
    let pi = (PixImage.Create @"C:\temp\blatif.tiff").ToPixImage<byte>()
    Log.line "size %A" pi.Size
    let border = 16
    let tileSize =
        let nParts = float pi.Size.X / float pi.Size.Y |> round |> int
        let sx = float pi.Size.X / float nParts |> ceil |> int

        V2i(sx, pi.Size.Y)

    let tileTextures = 
        let tiles = List<_>()
        let mutable o = 0
        while o < pi.Size.X do

            let bMin = o - border
            let bMax = o + tileSize.X - 1 + border

            let rMin = max bMin 0
            let rMax = min bMax (pi.Size.X - 1)
            let rSize = rMax - rMin + 1

            let tMin = o
            let tMax = tMin + min tileSize.X (pi.Size.X - o) - 1

            let bl = tMin - rMin
            let br = rMax - tMax

            let vol = 
                pi.Volume.SubVolume(
                    V3l(rMin,0,0),
                    V3l(rSize,tileSize.Y,pi.ChannelCount)
                )

            let minTc = float bl / float rSize
            let maxTc = 1.0 - float br / float rSize

            let tcScale = maxTc - minTc
            let tcOffset = minTc
            // (0,1) * (maxTc - minTc) + minTc

            let tile = PixImage<byte>(pi.Format, vol)
            let trafo = 
                Trafo3d.Scale(V3d(float (tMax - tMin + 1), float tileSize.Y, 1.0)) *
                Trafo3d.Translation(V3d(float tMin,0.0,0.0))

            let tt = V4d(tcOffset, 0.0, tcScale, 1.0)

            tiles.Add(tile, trafo, tt)
            
            o <- o+tileSize.X
        tiles

    

    let quadGeometry =
        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,
            IndexArray = ([|0;1;2; 0;2;3|] :> Array),
            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, pos :> Array
                    DefaultSemantic.Colors, [| C4b.Red; C4b.Green; C4b.Blue; C4b.Yellow |] :> Array
                    DefaultSemantic.DiffuseColorCoordinates, [| V2f.OO; V2f.IO; V2f.II; V2f.OI |] :> Array
                ]
        )
        |> Sg.ofIndexedGeometry
    let initialView = CameraView.lookAt (V3d(6,6,6)) V3d.Zero V3d.OOI
    let view = initialView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    let proj = win.Sizes |> AVal.map (fun s -> Frustum.perspective 70.0 0.1 1000.0 (float s.X / float s.Y))

    let tilesSg =
        tileTextures
        |> Seq.map (fun (tile,trafo, offScale) -> 
            let tex = 
                PixTexture2d(PixImageMipMap [| tile :> PixImage |], true) :> ITexture
                |> AVal.constant
            quadGeometry
            |> Sg.transform trafo
            |> Sg.diffuseTexture tex
            |> Sg.uniform' "OffsetAndScale" offScale
        ) |> Sg.ofSeq
        
    let fill = cval FillMode.Fill

    let sg =
        tilesSg
            |> Sg.scale (1.0/100.0)
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                Shader.tex |> toEffect
               ]
            |> Sg.viewTrafo (view |> AVal.map CameraView.viewTrafo)
            |> Sg.projTrafo (proj |> AVal.map Frustum.projTrafo)
            |> Sg.fillMode fill
            |> Sg.multisample' true
    let task =
        app.Runtime.CompileRender(win.FramebufferSignature, sg)

    win.Keyboard.DownWithRepeats.Values.Add (fun k ->
        match k with
        | Keys.Space -> 
            transact (fun () ->
                match fill.Value with
                | FillMode.Fill -> fill.Value <- FillMode.Line
                | _ -> fill.Value <- FillMode.Fill

            )
        | _ -> ()   
    )

    win.RenderTask <- task
    win.Run()
    0
