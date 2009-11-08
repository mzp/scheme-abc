open Swflib.AbcType

let cpool = {
  Swflib.AbcType.int = [];
  uint          = [];
  double        = [];
  string        = [];
  namespace     = [];
  namespace_set = [];
  multiname     = [];
}

let abc = {
  Swflib.AbcType.cpool         = cpool;
  method_info   = [];
  metadata      = [];
  classes       = [];
  instances     = [];
  scripts       = [];
  method_bodies = []
}

let info = {
  params       = [];
  return       = 0;
  method_name  = 0;
  method_flags = []
}

let body = {
  method_sig       = 0;
  max_stack        = 0;
  local_count      = 0;
  init_scope_depth = 0;
  max_scope_depth  = 0;
  code             = [];
  exceptions       = [];
  method_traits    = []
}

let class_ = {
  cinit=0;
  class_traits=[];
}

let instance={
  instance_name=0;
  super_name=0;
  instance_flags=[];
  interfaces=[];
  iinit=0;
  instance_traits=[]
}

