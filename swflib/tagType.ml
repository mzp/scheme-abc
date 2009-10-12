
type file_attrs = {
  is_metadata : bool;
  is_as3      : bool;
  use_network : bool;
}
type alist = (int * string) list

type t = [
| `PlaceObject of int * int * SwfType.matrix
| `FrameLabel of string * bool
| `Protect
| `End
| `ExportAssets of alist
| `ImportAssets of string * alist
| `EnableDebuger of string
| `EnableDebugger2 of string
| `ScriptLimits of int * int
| `SetTabIndex of int * int
| `FileAttributes of file_attrs
| `ImportAssets2 of string * alist
| `SymbolClass of alist
| `Metadata of string
| `DefineScalingGrid of SwfType.rect
| `DefineSceneAndFrameLabelData of (int * string) list * (int * string) list
| `ShowFrame
| `SetBackgroundColor of int * int * int
| `DoABC of bool * string * int list
]
