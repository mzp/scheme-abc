let element name attrs children =
  Xml.Element (name,attrs,children)

let elem name children =
  element name [] children

let attr name attrs =
  element name attrs []

let pcdata x =
  Xml.PCData x


let rec normalize =
  function
      Xml.Element (name,attrs,children) ->
	Xml.Element (name,
		     List.sort (fun (a,_) (b,_) -> compare a b) attrs,
		     List.map normalize children)
    | Xml.PCData _ as x ->
	x
