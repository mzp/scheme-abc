open Base
open SwfType
open TagType
open SwfBaseOut

let alist xs =
  let symbol (id,name) =
    [`Ui16 id; `Str name] in
    List.concat [
      [`Ui16 (List.length xs)];
      HList.concat_map symbol xs]

let tag id body =
  (id,body)

let to_base : TagType.t -> int*SwfBaseOut.s list = function
(*    `PlaceObject (id,depth,matrix) ->
      tag 4 [
	`Ui16 id;
	`Ui16 depth;
	`Matrix matrix
      ]*)
  | `FrameLabel (name,anchor) ->
      if anchor then
	tag 43 [`Str name; `Ui8 1]
      else
	tag 43 [`Str name]
  | `Protect ->
      tag 24 []
  | `End ->
      tag 0 []
  | `ExportAssets xs ->
      tag 56 @@ alist xs
  | `ImportAssets (url, xs) ->
      tag 57 @@ (`Str url)::alist xs
  | `EnableDebugger passwd ->
      tag 58 [`Str passwd]
  | `EnableDebugger2 passwd ->
      tag 64 [`Ui16 0; `Str passwd]
  | `ScriptLimits (recursion, timeout) ->
      tag 65 [`Ui16 recursion; `Ui16 timeout]
  | `SetTabIndex (depth, order) ->
      tag 66 [`Ui16 depth; `Ui16 order]
  | `ShowFrame ->
      tag 1 []
  | `SetBackgroundColor(r,g,b) ->
      tag 9 [`RGB(r,g,b)]
  | `FileAttributes {is_metadata; is_as3; use_network} ->
      tag 69 [
	`Bits [
	  UB(3 , 0);
	  UB(1 , if is_metadata then 1 else 0);
	  UB(1 , if is_as3 then 1 else 0);
	  UB(2 , 0);
	  UB(1 , if use_network then 1 else 0);
	  UB(24, 0)
	]]
  | `ImportAssets2 (url, xs) ->
      tag 71 @@ (`Str url)::`Ui8 1::`Ui8 0::alist xs
  | `SymbolClass xs ->
      tag 76 @@ alist xs
  | `Metadata xml ->
      tag 77 [`Str xml]
  | `DefineScalingGrid (id, {left;right;top;bottom}) ->
      tag 78 [`Ui16 id; `Rect (left,right,top,bottom)]
  | `DefineSceneAndFrameLabelData (scenes, frames) ->
      let bytes xs =
	(`EUi32 (Int32.of_int @@ List.length xs))::
	  HList.concat_map (fun (x,y) -> [`EUi32 x; `Str y]) xs in
      tag 86 @@ List.concat [
	bytes scenes;
	bytes frames;
      ]
  | `DoABC (lazyInit, name, data) ->
      tag 82 @@ List.concat [
	[if lazyInit then `Ui32 1l else `Ui32 0l;
	 `Str name];
	List.map (fun n -> `Ui8 n) data ]

