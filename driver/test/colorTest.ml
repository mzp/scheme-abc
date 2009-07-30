open Base
open Color
open OUnit

let ok x y =
  OUnit.assert_equal ~printer:Std.dump
    x y

let _  = ("color.ml" >::: [
	    "#-format" >::
	      (fun () ->
		 ok {Color.red=255;
		     green=255;
		     blue=255;
		     alpha=1.0} @@
		   Color.parse "#ffffff";
		 ok {Color.red=0x00;
		     green=0x22;
		     blue=0x33;
		     alpha=1.0} @@
		   Color.parse "#002233";
	      );
	    "rgb-format" >::
	      (fun () ->
		 ok {Color.red=255;
		     green=255;
		     blue=255;
		     alpha=1.0} @@
		   Color.parse "rgb(255,255,255)";
		 ok {Color.red=00;
		     green=22;
		     blue=33;
		     alpha=1.0} @@
		   Color.parse "rgb(00,22,33)";
	      );
	    "name" >::
	      (fun () ->
		 List.iter (fun (c,name) ->
			      ok c @@ Color.parse name;
			      ok c @@ Color.parse @@ String.uppercase name;
			      ok c @@ Color.parse @@ String.capitalize name)
		   [red,"red";
		    lime,"lime";
		    blue,"blue";
	            white,"white";
		    maroon,"maroon";
		    green,"green";
		    navy,"navy";
		    silver,"silver";
		    yellow,"yellow";
		    aqua,"aqua";
		    fuchsia,"fuchsia";
		    gray,"gray";
		    olive,"olive";
		    teal,"teal";
		    purple,"purple";
		    black,"black"])
	  ]) +> run_test_tt
