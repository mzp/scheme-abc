open Base
open Swflib.SwfType

(*<swf version="9" compressed="1">
  <Header framerate="24" frames="1">
    <size>
      <Rectangle left="0" right="__WIDTH__" top="0" bottom="__HEIGHT__"/>
    </size>
    <tags>
      <FileAttributes hasMetaData="1" allowABC="1" suppressCrossDomainCaching="0" swfRelativeURLs="0" useNetwork="1"/>
      <SetBackgroundColor>
        <color>
          <Color red="__BG_RED__" green="__BG_GREEN__" blue="__BG_BLUE__"/>
        </color>
      </SetBackgroundColor>
      <FrameLabel label="__MAIN_CLASS__">
        <flags/>
      </FrameLabel>
      <DoABCDefine flags="1" name="frame1">
        <actions>
include(__ABCX__)
</actions>
      </DoABCDefine>
      <SymbolClass>
        <symbols>
          <Symbol objectID="0" name="__MAIN_CLASS__"/>
        </symbols>
      </SymbolClass>
      <ShowFrame/>
      <End/>
    </tags>
  </Header>
</swf>
*)
let _ =
  print_endline "hello"
