namespace GoBoard

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

[<JavaScript>]
module ImageLoader =
    type ImageLoader() =
        let mutable imgCounter = 0
        let mutable loadCounter = 0
        let mutable loadRequested = false
        let mutable onload = fun () -> ()
        let onLoadEnd () = 
            loadCounter <- loadCounter + 1
            if (loadRequested && loadCounter = imgCounter) then onload()
        member x.CreateImage(src) =
            imgCounter <- imgCounter + 1 
            imgAttr [on.loadEnd (fun _ _ -> onLoadEnd()); attr.src src] [] |> DocExtensions.GetDom
        member x.Load(f) =
            onload <- f
            loadRequested <- true
            if loadCounter = imgCounter then onload()

(*
      let imgLoader = ImageLoader()
      let img1 = imgLoader.CreateImage("/whitestone.jpg")
      imgLoader.Load (fun () -> ())
*)
