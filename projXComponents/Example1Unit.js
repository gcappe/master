var pas = {};

var rtl = {

  quiet: false,
  debug_load_units: false,
  debug_rtti: false,

  debug: function(){
    if (rtl.quiet || !console || !console.log) return;
    console.log(arguments);
  },

  error: function(s){
    rtl.debug('Error: ',s);
    throw s;
  },

  warn: function(s){
    rtl.debug('Warn: ',s);
  },

  hasString: function(s){
    return rtl.isString(s) && (s.length>0);
  },

  isArray: function(a) {
    return Array.isArray(a);
  },

  isFunction: function(f){
    return typeof(f)==="function";
  },

  isModule: function(m){
    return rtl.isObject(m) && rtl.hasString(m.$name) && (pas[m.$name]===m);
  },

  isImplementation: function(m){
    return rtl.isObject(m) && rtl.isModule(m.$module) && (m.$module.$impl===m);
  },

  isNumber: function(n){
    return typeof(n)==="number";
  },

  isObject: function(o){
    var s=typeof(o);
    return (typeof(o)==="object") && (o!=null);
  },

  isString: function(s){
    return typeof(s)==="string";
  },

  getNumber: function(n){
    return typeof(n)==="number"?n:NaN;
  },

  getChar: function(c){
    return ((typeof(c)==="string") && (c.length===1)) ? c : "";
  },

  getObject: function(o){
    return ((typeof(o)==="object") || (typeof(o)==='function')) ? o : null;
  },

  isPasClass: function(type){
    return (rtl.isObject(type) && type.hasOwnProperty('$classname') && rtl.isObject(type.$module));
  },

  isPasClassInstance: function(type){
    return (rtl.isObject(type) && rtl.isPasClass(type.$class));
  },

  m_loading: 0,
  m_loading_intf: 1,
  m_intf_loaded: 2,
  m_loading_impl: 3, // loading all used unit
  m_initializing: 4, // running initialization
  m_initialized: 5,

  module: function(module_name, intfuseslist, intfcode, impluseslist, implcode){
    if (rtl.debug_load_units) rtl.debug('rtl.module name="'+module_name+'" intfuses='+intfuseslist+' impluses='+impluseslist+' hasimplcode='+rtl.isFunction(implcode));
    if (!rtl.hasString(module_name)) rtl.error('invalid module name "'+module_name+'"');
    if (!rtl.isArray(intfuseslist)) rtl.error('invalid interface useslist of "'+module_name+'"');
    if (!rtl.isFunction(intfcode)) rtl.error('invalid interface code of "'+module_name+'"');
    if (!(impluseslist==undefined) && !rtl.isArray(impluseslist)) rtl.error('invalid implementation useslist of "'+module_name+'"');
    if (!(implcode==undefined) && !rtl.isFunction(implcode)) rtl.error('invalid implementation code of "'+module_name+'"');

    if (pas[module_name])
      rtl.error('module "'+module_name+'" is already registered');

    var module = pas[module_name] = {
      $name: module_name,
      $intfuseslist: intfuseslist,
      $impluseslist: impluseslist,
      $state: rtl.m_loading,
      $intfcode: intfcode,
      $implcode: implcode,
      $impl: null,
      $rtti: Object.create(rtl.tSectionRTTI),
    };
    module.$rtti.$module = module;
    if (implcode) module.$impl = {
      $module: module,
      $rtti: module.$rtti,
    };
  },

  exitcode: 0,

  run: function(module_name){
  
    function doRun(){
      if (!rtl.hasString(module_name)) module_name='program';
      if (rtl.debug_load_units) rtl.debug('rtl.run module="'+module_name+'"');
      rtl.initRTTI();
      var module = pas[module_name];
      if (!module) rtl.error('rtl.run module "'+module_name+'" missing');
      rtl.loadintf(module);
      rtl.loadimpl(module);
      if (module_name=='program'){
        if (rtl.debug_load_units) rtl.debug('running $main');
        var r = pas.program.$main();
        if (rtl.isNumber(r)) rtl.exitcode = r;
      }
    }
    
    if (rtl.showUncaughtExceptions) {
      try{
        doRun();
      } catch(re) {
        var errMsg = re.hasOwnProperty('$class') ? re.$class.$classname : '';
	    errMsg +=  ((errMsg) ? ': ' : '') + (re.hasOwnProperty('fMessage') ? re.fMessage : re);
        alert('Uncaught Exception : '+errMsg);
        rtl.exitCode = 216;
      }
    } else {
      doRun();
    }
    return rtl.exitcode;
  },

  loadintf: function(module){
    if (module.$state>rtl.m_loading_intf) return; // already finished
    if (rtl.debug_load_units) rtl.debug('loadintf: "'+module.$name+'"');
    if (module.$state===rtl.m_loading_intf)
      rtl.error('unit cycle detected "'+module.$name+'"');
    module.$state=rtl.m_loading_intf;
    // load interfaces of interface useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadintf);
    // run interface
    if (rtl.debug_load_units) rtl.debug('loadintf: run intf of "'+module.$name+'"');
    module.$intfcode(module.$intfuseslist);
    // success
    module.$state=rtl.m_intf_loaded;
    // Note: units only used in implementations are not yet loaded (not even their interfaces)
  },

  loaduseslist: function(module,useslist,f){
    if (useslist==undefined) return;
    for (var i in useslist){
      var unitname=useslist[i];
      if (rtl.debug_load_units) rtl.debug('loaduseslist of "'+module.$name+'" uses="'+unitname+'"');
      if (pas[unitname]==undefined)
        rtl.error('module "'+module.$name+'" misses "'+unitname+'"');
      f(pas[unitname]);
    }
  },

  loadimpl: function(module){
    if (module.$state>=rtl.m_loading_impl) return; // already processing
    if (module.$state<rtl.m_intf_loaded) rtl.error('loadimpl: interface not loaded of "'+module.$name+'"');
    if (rtl.debug_load_units) rtl.debug('loadimpl: load uses of "'+module.$name+'"');
    module.$state=rtl.m_loading_impl;
    // load interfaces of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadintf);
    // load implementation of interfaces useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadimpl);
    // load implementation of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadimpl);
    // Note: At this point all interfaces used by this unit are loaded. If
    //   there are implementation uses cycles some used units might not yet be
    //   initialized. This is by design.
    // run implementation
    if (rtl.debug_load_units) rtl.debug('loadimpl: run impl of "'+module.$name+'"');
    if (rtl.isFunction(module.$implcode)) module.$implcode(module.$impluseslist);
    // run initialization
    if (rtl.debug_load_units) rtl.debug('loadimpl: run init of "'+module.$name+'"');
    module.$state=rtl.m_initializing;
    if (rtl.isFunction(module.$init)) module.$init();
    // unit initialized
    module.$state=rtl.m_initialized;
  },

  createCallback: function(scope, fn){
    var cb;
    if (typeof(fn)==='string'){
      cb = function(){
        return scope[fn].apply(scope,arguments);
      };
    } else {
      cb = function(){
        return fn.apply(scope,arguments);
      };
    };
    cb.scope = scope;
    cb.fn = fn;
    return cb;
  },

  cloneCallback: function(cb){
    return rtl.createCallback(cb.scope,cb.fn);
  },

  eqCallback: function(a,b){
    // can be a function or a function wrapper
    if (a==b){
      return true;
    } else {
      return (a!=null) && (b!=null) && (a.fn) && (a.scope===b.scope) && (a.fn==b.fn);
    }
  },

  initClass: function(c,parent,name,initfn){
    parent[name] = c;
    c.$classname = name;
    if ((parent.$module) && (parent.$module.$impl===parent)) parent=parent.$module;
    c.$parent = parent;
    c.$fullname = parent.$name+'.'+name;
    if (rtl.isModule(parent)){
      c.$module = parent;
      c.$name = name;
    } else {
      c.$module = parent.$module;
      c.$name = parent.name+'.'+name;
    };
    // rtti
    if (rtl.debug_rtti) rtl.debug('initClass '+c.$fullname);
    var t = c.$module.$rtti.$Class(c.$name,{ "class": c, module: parent });
    c.$rtti = t;
    if (rtl.isObject(c.$ancestor)) t.ancestor = c.$ancestor.$rtti;
    if (!t.ancestor) t.ancestor = null;
    // init members
    initfn.call(c);
  },

  createClass: function(parent,name,ancestor,initfn){
    // create a normal class,
    // ancestor must be null or a normal class,
    // the root ancestor can be an external class
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
      // Note:
      // if root is an "object" then c.$ancestor === Object.getPrototypeOf(c)
      // if root is a "function" then c.$ancestor === c.__proto__, Object.getPrototypeOf(c) returns the root
    } else {
      c = {};
      c.$create = function(fnname,args){
        if (args == undefined) args = [];
        var o = Object.create(this);
        o.$class = this; // Note: o.$class === Object.getPrototypeOf(o)
        o.$init();
        try{
          o[fnname].apply(o,args);
          o.AfterConstruction();
        } catch($e){
          o.$destroy;
          throw $e;
        }
        return o;
      };
      c.$destroy = function(fnname){
        this.BeforeDestruction();
        this[fnname]();
        this.$final;
      };
    };
    rtl.initClass(c,parent,name,initfn);
  },

  createClassExt: function(parent,name,ancestor,newinstancefnname,initfn){
    // Create a class using an external ancestor.
    // If newinstancefnname is given, use that function to create the new object.
    // If exist call BeforeDestruction and AfterConstruction.
    var c = null;
    c = Object.create(ancestor);
    c.$create = function(fnname,args){
      if (args == undefined) args = [];
      var o = null;
      if (newinstancefnname.length>0){
        o = this[newinstancefnname](fnname,args);
      } else {
        o = Object.create(this);
      }
      o.$class = this; // Note: o.$class === Object.getPrototypeOf(o)
      o.$init();
      try{
        o[fnname].apply(o,args);
        if (o.AfterConstruction) o.AfterConstruction();
      } catch($e){
        o.$destroy;
        throw $e;
      }
      return o;
    };
    c.$destroy = function(fnname){
      if (this.BeforeDestruction) this.BeforeDestruction();
      this[fnname]();
      this.$final;
    };
    rtl.initClass(c,parent,name,initfn);
  },

  tObjectDestroy: "Destroy",

  free: function(obj,name){
    if (obj[name]==null) return;
    obj[name].$destroy(rtl.tObjectDestroy);
    obj[name]=null;
  },

  freeLoc: function(obj){
    if (obj==null) return;
    obj.$destroy(rtl.tObjectDestroy);
    return null;
  },

  is: function(instance,type){
    return type.isPrototypeOf(instance) || (instance===type);
  },

  isExt: function(instance,type,mode){
    // mode===1 means instance must be a Pascal class instance
    // mode===2 means instance must be a Pascal class
    // Notes:
    // isPrototypeOf and instanceof return false on equal
    // isPrototypeOf does not work for Date.isPrototypeOf(new Date())
    //   so if isPrototypeOf is false test with instanceof
    // instanceof needs a function on right side
    if (instance == null) return false; // Note: ==null checks for undefined too
    if ((typeof(type) !== 'object') && (typeof(type) !== 'function')) return false;
    if (instance === type){
      if (mode===1) return false;
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if (type.isPrototypeOf && type.isPrototypeOf(instance)){
      if (mode===1) return rtl.isPasClassInstance(instance);
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if ((typeof type == 'function') && (instance instanceof type)) return true;
    return false;
  },

  EInvalidCast: null,

  raiseEInvalidCast: function(){
    if (rtl.EInvalidCast){
      if (rtl.EInvalidCast.Create){
        throw rtl.EInvalidCast.$create("Create");
      } else {
        throw rtl.EInvalidCast.$create("create");
      }
    } else {
      throw "invalid type cast";
    }
  },

  as: function(instance,type){
    if((instance === null) || rtl.is(instance,type)) return instance;
    rtl.raiseEInvalidCast();
  },

  asExt: function(instance,type,mode){
    if((instance === null) || rtl.isExt(instance,type,mode)) return instance;
    rtl.raiseEInvalidCast();
  },

  checkMethodCall: function(obj,type){
    if (rtl.isObject(obj) && rtl.is(obj,type)) return;
    rtl.raiseEInvalidCast();
  },

  raiseRangeCheck: function(){
    var m = pas.sysutils || pas.SysUtils;
    if (m){
      var t = m.ERangeError || m.erangeerror;
      if (rtl.isPasClass(t)){
        var f = 'Create';
        if (rtl.isFunction(t[f])){
          throw t.$create(f);
        } else {
          throw t.$create('create');
        }
      }
    }
    throw 'range error';
  },

  rc: function(i,minval,maxval){
    // range check integer
    if ((Math.floor(i)===i) && (i>=minval) && (i<=maxval)) return i;
    rtl.raiseRangeCheck();
  },

  length: function(arr){
    return (arr == null) ? 0 : arr.length;
  },

  arraySetLength: function(arr,defaultvalue,newlength){
    // multi dim: (arr,defaultvalue,dim1,dim2,...)
    if (arr == null) arr = [];
    var p = arguments;
    function setLength(a,argNo){
      var oldlen = a.length;
      var newlen = p[argNo];
      if (oldlen!==newlength){
        a.length = newlength;
        if (argNo === p.length-1){
          if (rtl.isArray(defaultvalue)){
            for (var i=oldlen; i<newlen; i++) a[i]=[]; // nested array
          } else if (rtl.isFunction(defaultvalue)){
            for (var i=oldlen; i<newlen; i++) a[i]=new defaultvalue(); // e.g. record
          } else if (rtl.isObject(defaultvalue)) {
            for (var i=oldlen; i<newlen; i++) a[i]={}; // e.g. set
          } else {
            for (var i=oldlen; i<newlen; i++) a[i]=defaultvalue;
          }
        } else {
          for (var i=oldlen; i<newlen; i++) a[i]=[]; // nested array
        }
      }
      if (argNo < p.length-1){
        // multi argNo
        for (var i=0; i<newlen; i++) a[i]=setLength(a[i],argNo+1);
      }
      return a;
    }
    return setLength(arr,2);
  },

  arrayClone: function(type,src,srcpos,end,dst,dstpos){
    // type: 0 for references, "refset" for calling refSet(), a function for new type()
    // src must not be null
    // This function does not range check.
    if (rtl.isFunction(type)){
      for (; srcpos<end; srcpos++) dst[dstpos++] = new type(src[srcpos]); // clone record
    } else if(isString(type) && (type === 'refSet')) {
      for (; srcpos<end; srcpos++) dst[dstpos++] = refSet(src[srcpos]); // ref set
    }  else {
      for (; srcpos<end; srcpos++) dst[dstpos++] = src[srcpos]; // reference
    };
  },

  arrayConcat: function(type){
    // type: see rtl.arrayClone
    var a = [];
    var l = 0;
    for (var i=1; i<arguments.length; i++) l+=arguments[i].length;
    a.length = l;
    l=0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src == null) continue;
      rtl.arrayClone(type,src,0,src.length,a,l);
      l+=src.length;
    };
    return a;
  },

  arrayCopy: function(type, srcarray, index, count){
    // type: see rtl.arrayClone
    // if count is missing, use srcarray.length
    if (srcarray == null) return [];
    if (index < 0) index = 0;
    if (count === undefined) count=srcarray.length;
    var end = index+count;
    if (end>scrarray.length) end = scrarray.length;
    if (index>=end) return [];
    if (type===0){
      return srcarray.slice(index,end);
    } else {
      var a = [];
      a.length = end-index;
      rtl.arrayClone(type,srcarray,index,end,a,0);
      return a;
    }
  },

  setCharAt: function(s,index,c){
    return s.substr(0,index)+c+s.substr(index+1);
  },

  getResStr: function(mod,name){
    var rs = mod.$resourcestrings[name];
    return rs.current?rs.current:rs.org;
  },

  createSet: function(){
    var s = {};
    for (var i=0; i<arguments.length; i++){
      if (arguments[i]!=null){
        s[arguments[i]]=true;
      } else {
        var first=arguments[i+=1];
        var last=arguments[i+=1];
        for(var j=first; j<=last; j++) s[j]=true;
      }
    }
    return s;
  },

  cloneSet: function(s){
    var r = {};
    for (var key in s) r[key]=true;
    return r;
  },

  refSet: function(s){
    s.$shared = true;
    return s;
  },

  includeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    s[enumvalue] = true;
    return s;
  },

  excludeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    delete s[enumvalue];
    return s;
  },

  diffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  unionSet: function(s,t){
    var r = {};
    for (var key in s) r[key]=true;
    for (var key in t) r[key]=true;
    delete r.$shared;
    return r;
  },

  intersectSet: function(s,t){
    var r = {};
    for (var key in s) if (t[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  symDiffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    for (var key in t) if (!s[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  eqSet: function(s,t){
    for (var key in s) if (!t[key] && (key!='$shared')) return false;
    for (var key in t) if (!s[key] && (key!='$shared')) return false;
    return true;
  },

  neSet: function(s,t){
    return !rtl.eqSet(s,t);
  },

  leSet: function(s,t){
    for (var key in s) if (!t[key] && (key!='$shared')) return false;
    return true;
  },

  geSet: function(s,t){
    for (var key in t) if (!s[key] && (key!='$shared')) return false;
    return true;
  },

  strSetLength: function(s,newlen){
    var oldlen = s.length;
    if (oldlen > newlen){
      return s.substring(0,newlen);
    } else if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return s+' '.repeat(newlen-oldlen);
    } else {
       while (oldlen<newlen){
         s+=' ';
         oldlen++;
       };
       return s;
    }
  },

  spaceLeft: function(s,width){
    var l=s.length;
    if (l>=width) return s;
    if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return ' '.repeat(width-l) + s;
    } else {
      while (l<width){
        s=' '+s;
        l++;
      };
    };
  },

  floatToStr : function(d,w,p){
    // input 1-3 arguments: double, width, precision
    if (arguments.length>2){
      return rtl.spaceLeft(d.toFixed(p),w);
    } else {
	  // exponent width
	  var pad = "";
	  var ad = Math.abs(d);
	  if (ad<1.0e+10) {
		pad='00';
	  } else if (ad<1.0e+100) {
		pad='0';
      }  	
	  if (arguments.length<2) {
	    w=9;		
      } else if (w<9) {
		w=9;
      }		  
      var p = w-8;
      var s=(d>0 ? " " : "" ) + d.toExponential(p);
      s=s.replace(/e(.)/,'E$1'+pad);
      return rtl.spaceLeft(s,w);
    }
  },

  initRTTI: function(){
    if (rtl.debug_rtti) rtl.debug('initRTTI');

    // base types
    rtl.tTypeInfo = { name: "tTypeInfo" };
    function newBaseTI(name,kind,ancestor){
      if (!ancestor) ancestor = rtl.tTypeInfo;
      if (rtl.debug_rtti) rtl.debug('initRTTI.newBaseTI "'+name+'" '+kind+' ("'+ancestor.name+'")');
      var t = Object.create(ancestor);
      t.name = name;
      t.kind = kind;
      rtl[name] = t;
      return t;
    };
    function newBaseInt(name,minvalue,maxvalue,ordtype){
      var t = newBaseTI(name,1 /* tkInteger */,rtl.tTypeInfoInteger);
      t.minvalue = minvalue;
      t.maxvalue = maxvalue;
      t.ordtype = ordtype;
      return t;
    };
    newBaseTI("tTypeInfoInteger",1 /* tkInteger */);
    newBaseInt("shortint",-0x80,0x7f,0);
    newBaseInt("byte",0,0xff,1);
    newBaseInt("smallint",-0x8000,0x7fff,2);
    newBaseInt("word",0,0xffff,3);
    newBaseInt("longint",-0x80000000,0x7fffffff,4);
    newBaseInt("longword",0,0xffffffff,5);
    newBaseInt("nativeint",-0x10000000000000,0xfffffffffffff,6);
    newBaseInt("nativeuint",0,0xfffffffffffff,7);
    newBaseTI("char",2 /* tkChar */);
    newBaseTI("string",3 /* tkString */);
    newBaseTI("tTypeInfoEnum",4 /* tkEnumeration */,rtl.tTypeInfoInteger);
    newBaseTI("tTypeInfoSet",5 /* tkSet */);
    newBaseTI("double",6 /* tkDouble */);
    newBaseTI("boolean",7 /* tkBool */);
    newBaseTI("tTypeInfoProcVar",8 /* tkProcVar */);
    newBaseTI("tTypeInfoMethodVar",9 /* tkMethod */,rtl.tTypeInfoProcVar);
    newBaseTI("tTypeInfoArray",10 /* tkArray */);
    newBaseTI("tTypeInfoDynArray",11 /* tkDynArray */);
    newBaseTI("tTypeInfoPointer",15 /* tkPointer */);
    var t = newBaseTI("pointer",15 /* tkPointer */,rtl.tTypeInfoPointer);
    t.reftype = null;
    newBaseTI("jsvalue",16 /* tkJSValue */);
    newBaseTI("tTypeInfoRefToProcVar",17 /* tkRefToProcVar */,rtl.tTypeInfoProcVar);

    // member kinds
    rtl.tTypeMember = {};
    function newMember(name,kind){
      var m = Object.create(rtl.tTypeMember);
      m.name = name;
      m.kind = kind;
      rtl[name] = m;
    };
    newMember("tTypeMemberField",1); // tmkField
    newMember("tTypeMemberMethod",2); // tmkMethod
    newMember("tTypeMemberProperty",3); // tmkProperty

    // base object for storing members: a simple object
    rtl.tTypeMembers = {};

    // tTypeInfoStruct - base object for tTypeInfoClass and tTypeInfoRecord
    var tis = newBaseTI("tTypeInfoStruct",0);
    tis.$addMember = function(name,ancestor,options){
      if (rtl.debug_rtti){
        if (!rtl.hasString(name) || (name.charAt()==='$')) throw 'invalid member "'+name+'", this="'+this.name+'"';
        if (!rtl.is(ancestor,rtl.tTypeMember)) throw 'invalid ancestor "'+ancestor+':'+ancestor.name+'", "'+this.name+'.'+name+'"';
        if ((options!=undefined) && (typeof(options)!='object')) throw 'invalid options "'+options+'", "'+this.name+'.'+name+'"';
      };
      var t = Object.create(ancestor);
      t.name = name;
      this.members[name] = t;
      this.names.push(name);
      if (rtl.isObject(options)){
        for (var key in options) if (options.hasOwnProperty(key)) t[key] = options[key];
      };
      return t;
    };
    tis.addField = function(name,type,options){
      var t = this.$addMember(name,rtl.tTypeMemberField,options);
      if (rtl.debug_rtti){
        if (!rtl.is(type,rtl.tTypeInfo)) throw 'invalid type "'+type+'", "'+this.name+'.'+name+'"';
      };
      t.typeinfo = type;
      this.fields.push(name);
      return t;
    };
    tis.addFields = function(){
      var i=0;
      while(i<arguments.length){
        var name = arguments[i++];
        var type = arguments[i++];
        if ((i<arguments.length) && (typeof(arguments[i])==='object')){
          this.addField(name,type,arguments[i++]);
        } else {
          this.addField(name,type);
        };
      };
    };
    tis.addMethod = function(name,methodkind,params,result,options){
      var t = this.$addMember(name,rtl.tTypeMemberMethod,options);
      t.methodkind = methodkind;
      t.procsig = rtl.newTIProcSig(params);
      t.procsig.resulttype = result?result:null;
      this.methods.push(name);
      return t;
    };
    tis.addProperty = function(name,flags,result,getter,setter,options){
      var t = this.$addMember(name,rtl.tTypeMemberProperty,options);
      t.flags = flags;
      t.typeinfo = result;
      t.getter = getter;
      t.setter = setter;
      // Note: in options: params, stored, defaultvalue
      if (rtl.isArray(t.params)) t.params = rtl.newTIParams(t.params);
      this.properties.push(name);
      if (!rtl.isString(t.stored)) t.stored = "";
      return t;
    };
    tis.getField = function(index){
      return this.members[this.fields[index]];
    };
    tis.getMethod = function(index){
      return this.members[this.methods[index]];
    };
    tis.getProperty = function(index){
      return this.members[this.properties[index]];
    };

    newBaseTI("tTypeInfoRecord",12 /* tkRecord */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClass",13 /* tkClass */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClassRef",14 /* tkClassRef */);
  },

  tSectionRTTI: {
    $module: null,
    $inherited: function(name,ancestor,o){
      if (rtl.debug_rtti){
        rtl.debug('tSectionRTTI.newTI "'+(this.$module?this.$module.$name:"(no module)")
          +'"."'+name+'" ('+ancestor.name+') '+(o?'init':'forward'));
      };
      var t = this[name];
      if (t){
        if (!t.$forward) throw 'duplicate type "'+name+'"';
        if (!ancestor.isPrototypeOf(t)) throw 'typeinfo ancestor mismatch "'+name+'" ancestor="'+ancestor.name+'" t.name="'+t.name+'"';
      } else {
        t = Object.create(ancestor);
        t.name = name;
        t.module = this.module;
        this[name] = t;
      }
      if (o){
        delete t.$forward;
        for (var key in o) if (o.hasOwnProperty(key)) t[key]=o[key];
      } else {
        t.$forward = true;
      }
      return t;
    },
    $Scope: function(name,ancestor,o){
      var t=this.$inherited(name,ancestor,o);
      t.members = {};
      t.names = [];
      t.fields = [];
      t.methods = [];
      t.properties = [];
      return t;
    },
    $TI: function(name,kind,o){ var t=this.$inherited(name,rtl.tTypeInfo,o); t.kind = kind; return t; },
    $Int: function(name,o){ return this.$inherited(name,rtl.tTypeInfoInteger,o); },
    $Enum: function(name,o){ return this.$inherited(name,rtl.tTypeInfoEnum,o); },
    $Set: function(name,o){ return this.$inherited(name,rtl.tTypeInfoSet,o); },
    $StaticArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoArray,o); },
    $DynArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoDynArray,o); },
    $ProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoProcVar,o); },
    $RefToProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoRefToProcVar,o); },
    $MethodVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoMethodVar,o); },
    $Record: function(name,o){ return this.$Scope(name,rtl.tTypeInfoRecord,o); },
    $Class: function(name,o){ return this.$Scope(name,rtl.tTypeInfoClass,o); },
    $ClassRef: function(name,o){ return this.$inherited(name,rtl.tTypeInfoClassRef,o); },
    $Pointer: function(name,o){ return this.$inherited(name,rtl.tTypeInfoPointer,o); },
  },

  newTIParam: function(param){
    // param is an array, 0=name, 1=type, 2=optional flags
    var t = {
      name: param[0],
      typeinfo: param[1],
      flags: (rtl.isNumber(param[2]) ? param[2] : 0),
    };
    return t;
  },

  newTIParams: function(list){
    // list: optional array of [paramname,typeinfo,optional flags]
    var params = [];
    if (rtl.isArray(list)){
      for (var i=0; i<list.length; i++) params.push(rtl.newTIParam(list[i]));
    };
    return params;
  },

  newTIProcSig: function(params,result,flags){
    var s = {
      params: rtl.newTIParams(params),
      resulttype: result,
      flags: flags
    };
    return s;
  },
}
rtl.module("System",[],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.LineEnding = "\n";
  this.sLineBreak = $mod.LineEnding;
  this.MaxSmallint = 32767;
  this.MinSmallint = -32768;
  this.MaxShortInt = 127;
  this.MinShortInt = -128;
  this.MaxByte = 0xFF;
  this.MaxWord = 0xFFFF;
  this.MaxLongint = 0x7fffffff;
  this.MaxCardinal = 0xffffffff;
  this.Maxint = 2147483647;
  this.IsMultiThread = false;
  this.TTextLineBreakStyle = {"0": "tlbsLF", tlbsLF: 0, "1": "tlbsCRLF", tlbsCRLF: 1, "2": "tlbsCR", tlbsCR: 2};
  rtl.createClass($mod,"TObject",null,function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
    };
    this.Destroy = function () {
    };
    this.Free = function () {
      this.$destroy("Destroy");
    };
    this.ClassType = function () {
      return this;
    };
    this.ClassNameIs = function (Name) {
      var Result = false;
      Result = $impl.SameText(Name,this.$classname);
      return Result;
    };
    this.InheritsFrom = function (aClass) {
      return (aClass!=null) && ((this==aClass) || aClass.isPrototypeOf(this));
    };
    this.AfterConstruction = function () {
    };
    this.BeforeDestruction = function () {
    };
    this.Equals = function (Obj) {
      var Result = false;
      Result = Obj === this;
      return Result;
    };
    this.ToString = function () {
      var Result = "";
      Result = this.$classname;
      return Result;
    };
  });
  this.DefaultTextLineBreakStyle = $mod.TTextLineBreakStyle.tlbsLF;
  this.IsConsole = false;
  this.OnParamCount = null;
  this.OnParamStr = null;
  this.ParamCount = function () {
    var Result = 0;
    if ($mod.OnParamCount != null) {
      Result = $mod.OnParamCount()}
     else Result = 0;
    return Result;
  };
  this.ParamStr = function (Index) {
    var Result = "";
    if ($mod.OnParamStr != null) {
      Result = $mod.OnParamStr(Index)}
     else if (Index === 0) {
      Result = "js"}
     else Result = "";
    return Result;
  };
  this.Frac = function (A) {
    return A % 1;
  };
  this.Odd = function (A) {
    return A&1 != 0;
  };
  this.Random = function (Range) {
    return Math.floor(Math.random()*Range);
  };
  this.Sqr = function (A) {
    return A*A;
  };
  this.Sqr$1 = function (A) {
    return A*A;
  };
  this.Trunc = function (A) {
    if (!Math.trunc) {
      Math.trunc = function(v) {
        v = +v;
        if (!isFinite(v)) return v;
        return (v - v % 1) || (v < 0 ? -0 : v === 0 ? v : 0);
      };
    }
    $mod.Trunc = Math.trunc;
    return Math.trunc(A);
  };
  this.Int = function (A) {
    var Result = 0.0;
    Result = Math.trunc(A);
    return Result;
  };
  this.Copy = function (S, Index, Size) {
    if (Index<1) Index = 1;
    return (Size>0) ? S.substring(Index-1,Index+Size-1) : "";
  };
  this.Copy$1 = function (S, Index) {
    if (Index<1) Index = 1;
    return S.substr(Index-1);
  };
  this.Delete = function (S, Index, Size) {
    var h = "";
    if (((Index < 1) || (Index > S.get().length)) || (Size <= 0)) return;
    h = S.get();
    S.set($mod.Copy(h,1,Index - 1) + $mod.Copy$1(h,Index + Size));
  };
  this.Pos = function (Search, InString) {
    return InString.indexOf(Search)+1;
  };
  this.Insert = function (Insertion, Target, Index) {
    var t = "";
    if (Insertion === "") return;
    t = Target.get();
    if (Index < 1) {
      Target.set(Insertion + t)}
     else if (Index > t.length) {
      Target.set(t + Insertion)}
     else Target.set(($mod.Copy(t,1,Index - 1) + Insertion) + $mod.Copy(t,Index,t.length));
  };
  this.upcase = function (c) {
    return c.toUpperCase();
  };
  this.val = function (S, NI, Code) {
    var x = 0.0;
    Code.set(0);
    x = Number(S);
    if (isNaN(x) || (x !== $mod.Int(x))) {
      Code.set(1)}
     else NI.set($mod.Trunc(x));
  };
  this.val$1 = function (S, SI, Code) {
    var X = 0.0;
    Code.set(0);
    X = Number(S);
    if (isNaN(X) || (X !== $mod.Int(X))) {
      Code.set(1)}
     else if ((X < -128) || (X > 127)) {
      Code.set(2)}
     else SI.set($mod.Trunc(X));
  };
  this.val$2 = function (S, B, Code) {
    var x = 0.0;
    Code.set(0);
    x = Number(S);
    if (isNaN(x) || (x !== $mod.Int(x))) {
      Code.set(1)}
     else if ((x < 0) || (x > 255)) {
      Code.set(2)}
     else B.set($mod.Trunc(x));
  };
  this.val$3 = function (S, SI, Code) {
    var x = 0.0;
    Code.set(0);
    x = Number(S);
    if (isNaN(x) || (x !== $mod.Int(x))) {
      Code.set(1)}
     else if ((x < -32768) || (x > 32767)) {
      Code.set(2)}
     else SI.set($mod.Trunc(x));
  };
  this.val$4 = function (S, W, Code) {
    var x = 0.0;
    Code.set(0);
    x = Number(S);
    if (isNaN(x)) {
      Code.set(1)}
     else if ((x < 0) || (x > 65535)) {
      Code.set(2)}
     else W.set($mod.Trunc(x));
  };
  this.val$5 = function (S, I, Code) {
    var x = 0.0;
    Code.set(0);
    x = Number(S);
    if (isNaN(x)) {
      Code.set(1)}
     else if (x > 2147483647) {
      Code.set(2)}
     else I.set($mod.Trunc(x));
  };
  this.val$6 = function (S, C, Code) {
    var x = 0.0;
    Code.set(0);
    x = Number(S);
    if (isNaN(x) || (x !== $mod.Int(x))) {
      Code.set(1)}
     else if ((x < 0) || (x > 4294967295)) {
      Code.set(2)}
     else C.set($mod.Trunc(x));
  };
  this.val$7 = function (S, d, Code) {
    var x = 0.0;
    x = Number(S);
    if (isNaN(x)) {
      Code.set(1)}
     else {
      Code.set(0);
      d.set(x);
    };
  };
  this.StringOfChar = function (c, l) {
    var Result = "";
    var i = 0;
    Result = "";
    for (var $l1 = 1, $end2 = l; $l1 <= $end2; $l1++) {
      i = $l1;
      Result = Result + c;
    };
    return Result;
  };
  this.Write = function () {
    var i = 0;
    for (var $l1 = 0, $end2 = rtl.length(arguments) - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      if ($impl.WriteCallBack != null) {
        $impl.WriteCallBack(arguments[i],false)}
       else $impl.WriteBuf = $impl.WriteBuf + ("" + arguments[i]);
    };
  };
  this.Writeln = function () {
    var i = 0;
    var l = 0;
    var s = "";
    l = rtl.length(arguments) - 1;
    if ($impl.WriteCallBack != null) {
      for (var $l1 = 0, $end2 = l; $l1 <= $end2; $l1++) {
        i = $l1;
        $impl.WriteCallBack(arguments[i],i === l);
      };
    } else {
      s = $impl.WriteBuf;
      for (var $l3 = 0, $end4 = l; $l3 <= $end4; $l3++) {
        i = $l3;
        s = s + ("" + arguments[i]);
      };
      console.log(s);
      $impl.WriteBuf = "";
    };
  };
  this.SetWriteCallBack = function (H) {
    var Result = null;
    Result = $impl.WriteCallBack;
    $impl.WriteCallBack = H;
    return Result;
  };
  this.Assigned = function (V) {
    return (V!=undefined) && (V!=null) && (!rtl.isArray(V) || (V.length > 0));
  };
  this.StrictEqual = function (A, B) {
    return A === B;
  };
  this.StrictInequal = function (A, B) {
    return A !== B;
  };
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.SameText = function (s1, s2) {
    return s1.toLowerCase() == s2.toLowerCase();
  };
  $impl.WriteBuf = "";
  $impl.WriteCallBack = null;
});
rtl.module("RTLConsts",["System"],function () {
  "use strict";
  var $mod = this;
  this.SArgumentMissing = 'Missing argument in format "%s"';
  this.SInvalidFormat = 'Invalid format specifier : "%s"';
  this.SInvalidArgIndex = 'Invalid argument index in format: "%s"';
  this.SListCapacityError = "List capacity (%s) exceeded.";
  this.SListCountError = "List count (%s) out of bounds.";
  this.SListIndexError = "List index (%s) out of bounds";
  this.SSortedListError = "Operation not allowed on sorted list";
  this.SDuplicateString = "String list does not allow duplicates";
  this.SErrFindNeedsSortedList = "Cannot use find on unsorted list";
  this.SInvalidName = 'Invalid component name: "%s"';
  this.SInvalidBoolean = '"%s" is not a valid boolean.';
  this.SDuplicateName = 'Duplicate component name: "%s"';
  this.SErrInvalidDate = 'Invalid date: "%s"';
  this.SErrInvalidTimeFormat = 'Invalid time format: "%s"';
  this.SInvalidDateFormat = 'Invalid date format: "%s"';
  this.SCantReadPropertyS = 'Cannot read property "%s"';
  this.SCantWritePropertyS = 'Cannot write property "%s"';
  this.SErrPropertyNotFound = 'Unknown property: "%s"';
  this.SIndexedPropertyNeedsParams = 'Indexed property "%s" needs parameters';
  this.SErrInvalidInteger = 'Invalid integer value: "%s"';
  this.SErrInvalidFloat = 'Invalid floating-point value: "%s"';
  this.SInvalidDateTime = "Invalid date-time value: %s";
  this.SErrInvalidDayOfWeek = "%d is not a valid day of the week";
  this.SErrInvalidTimeStamp = 'Invalid date\/timestamp : "%s"';
  this.SErrInvalidDateWeek = "%d %d %d is not a valid dateweek";
  this.SErrInvalidDayOfYear = "Year %d does not have a day number %d";
  this.SErrInvalidDateMonthWeek = "Year %d, month %d, Week %d and day %d is not a valid date.";
  this.SErrInvalidDayOfWeekInMonth = "Year %d Month %d NDow %d DOW %d is not a valid date";
  this.SInvalidJulianDate = "%f Julian cannot be represented as a DateTime";
  this.SErrInvalidHourMinuteSecMsec = "%d:%d:%d.%d is not a valid time specification";
});
rtl.module("Types",["System"],function () {
  "use strict";
  var $mod = this;
  this.TDirection = {"0": "FromBeginning", FromBeginning: 0, "1": "FromEnd", FromEnd: 1};
  this.TDuplicates = {"0": "dupIgnore", dupIgnore: 0, "1": "dupAccept", dupAccept: 1, "2": "dupError", dupError: 2};
  this.TSize = function (s) {
    if (s) {
      this.cx = s.cx;
      this.cy = s.cy;
    } else {
      this.cx = 0;
      this.cy = 0;
    };
    this.$equal = function (b) {
      return (this.cx === b.cx) && (this.cy === b.cy);
    };
  };
  this.TPoint = function (s) {
    if (s) {
      this.x = s.x;
      this.y = s.y;
    } else {
      this.x = 0;
      this.y = 0;
    };
    this.$equal = function (b) {
      return (this.x === b.x) && (this.y === b.y);
    };
  };
  this.TRect = function (s) {
    if (s) {
      this.Left = s.Left;
      this.Top = s.Top;
      this.Right = s.Right;
      this.Bottom = s.Bottom;
    } else {
      this.Left = 0;
      this.Top = 0;
      this.Right = 0;
      this.Bottom = 0;
    };
    this.$equal = function (b) {
      return (this.Left === b.Left) && ((this.Top === b.Top) && ((this.Right === b.Right) && (this.Bottom === b.Bottom)));
    };
  };
  this.EqualRect = function (r1, r2) {
    var Result = false;
    Result = (((r1.Left === r2.Left) && (r1.Right === r2.Right)) && (r1.Top === r2.Top)) && (r1.Bottom === r2.Bottom);
    return Result;
  };
  this.Rect = function (Left, Top, Right, Bottom) {
    var Result = new $mod.TRect();
    Result.Left = Left;
    Result.Top = Top;
    Result.Right = Right;
    Result.Bottom = Bottom;
    return Result;
  };
  this.Bounds = function (ALeft, ATop, AWidth, AHeight) {
    var Result = new $mod.TRect();
    Result.Left = ALeft;
    Result.Top = ATop;
    Result.Right = ALeft + AWidth;
    Result.Bottom = ATop + AHeight;
    return Result;
  };
  this.Point = function (x, y) {
    var Result = new $mod.TPoint();
    Result.x = x;
    Result.y = y;
    return Result;
  };
  this.PtInRect = function (aRect, p) {
    var Result = false;
    Result = (((p.y >= aRect.Top) && (p.y < aRect.Bottom)) && (p.x >= aRect.Left)) && (p.x < aRect.Right);
    return Result;
  };
  this.IntersectRect = function (aRect, R1, R2) {
    var Result = false;
    var lRect = new $mod.TRect();
    lRect = new $mod.TRect(R1);
    if (R2.Left > R1.Left) lRect.Left = R2.Left;
    if (R2.Top > R1.Top) lRect.Top = R2.Top;
    if (R2.Right < R1.Right) lRect.Right = R2.Right;
    if (R2.Bottom < R1.Bottom) lRect.Bottom = R2.Bottom;
    if ($mod.IsRectEmpty(lRect)) {
      aRect.set(new $mod.TRect($mod.Rect(0,0,0,0)));
      Result = false;
    } else {
      Result = true;
      aRect.set(new $mod.TRect(lRect));
    };
    return Result;
  };
  this.UnionRect = function (aRect, R1, R2) {
    var Result = false;
    var lRect = new $mod.TRect();
    lRect = new $mod.TRect(R1);
    if (R2.Left < R1.Left) lRect.Left = R2.Left;
    if (R2.Top < R1.Top) lRect.Top = R2.Top;
    if (R2.Right > R1.Right) lRect.Right = R2.Right;
    if (R2.Bottom > R1.Bottom) lRect.Bottom = R2.Bottom;
    if ($mod.IsRectEmpty(lRect)) {
      aRect.set(new $mod.TRect($mod.Rect(0,0,0,0)));
      Result = false;
    } else {
      aRect.set(new $mod.TRect(lRect));
      Result = true;
    };
    return Result;
  };
  this.IsRectEmpty = function (aRect) {
    var Result = false;
    Result = (aRect.Right <= aRect.Left) || (aRect.Bottom <= aRect.Top);
    return Result;
  };
  this.OffsetRect = function (aRect, DX, DY) {
    var Result = false;
    var $with1 = aRect.get();
    $with1.Left += DX;
    $with1.Top += DY;
    $with1.Right += DX;
    $with1.Bottom += DY;
    Result = true;
    return Result;
  };
  this.CenterPoint = function (aRect) {
    var Result = new $mod.TPoint();
    function Avg(a, b) {
      var Result = 0;
      if (a < b) {
        Result = a + ((b - a) >>> 1)}
       else Result = b + ((a - b) >>> 1);
      return Result;
    };
    Result.x = Avg(aRect.Left,aRect.Right);
    Result.y = Avg(aRect.Top,aRect.Bottom);
    return Result;
  };
  this.InflateRect = function (aRect, dx, dy) {
    var Result = false;
    var $with1 = aRect.get();
    $with1.Left -= dx;
    $with1.Top -= dy;
    $with1.Right += dx;
    $with1.Bottom += dy;
    Result = true;
    return Result;
  };
  this.Size = function (AWidth, AHeight) {
    var Result = new $mod.TSize();
    Result.cx = AWidth;
    Result.cy = AHeight;
    return Result;
  };
  this.Size$1 = function (aRect) {
    var Result = new $mod.TSize();
    Result.cx = aRect.Right - aRect.Left;
    Result.cy = aRect.Bottom - aRect.Top;
    return Result;
  };
});
rtl.module("JS",["System","Types"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"EJS",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FMessage = "";
    };
    this.Create$1 = function (Msg) {
      this.FMessage = Msg;
    };
  });
  this.TLocaleCompareOptions = function (s) {
    if (s) {
      this.localematched = s.localematched;
      this.usage = s.usage;
      this.sensitivity = s.sensitivity;
      this.ignorePunctuation = s.ignorePunctuation;
      this.numeric = s.numeric;
      this.caseFirst = s.caseFirst;
    } else {
      this.localematched = "";
      this.usage = "";
      this.sensitivity = "";
      this.ignorePunctuation = false;
      this.numeric = false;
      this.caseFirst = "";
    };
    this.$equal = function (b) {
      return (this.localematched === b.localematched) && ((this.usage === b.usage) && ((this.sensitivity === b.sensitivity) && ((this.ignorePunctuation === b.ignorePunctuation) && ((this.numeric === b.numeric) && (this.caseFirst === b.caseFirst)))));
    };
  };
  this.New = function (aElements) {
    var Result = null;
    var L = 0;
    var I = 0;
    var S = "";
    L = rtl.length(aElements);
    if ((L % 2) === 1) throw $mod.EJS.$create("Create$1",["Number of arguments must be even"]);
    I = 0;
    while (I < L) {
      if (!rtl.isString(aElements[I])) {
        S = String(I);
        throw $mod.EJS.$create("Create$1",[("Argument " + S) + " must be a string."]);
      };
      I += 2;
    };
    I = 0;
    Result = new Object();
    while (I < L) {
      S = "" + aElements[I];
      Result[S] = aElements[I + 1];
      I += 2;
    };
    return Result;
  };
  this.hasValue = function (v) {
    if(v){ return true; } else { return false; };
  };
  this.isBoolean = function (v) {
    return typeof(v) == 'boolean';
  };
  this.isCallback = function (v) {
    return rtl.isObject(v) && rtl.isObject(v.scope) && (rtl.isString(v.fn) || rtl.isFunction(v.fn));
  };
  this.isChar = function (v) {
    return (typeof(v)!="string") && (v.length==1);
  };
  this.isClass = function (v) {
    return (typeof(v)=="object") && (v!=null) && (v.$class == v);
  };
  this.isClassInstance = function (v) {
    return (typeof(v)=="object") && (v!=null) && (v.$class == Object.getPrototypeOf(v));
  };
  this.isInteger = function (v) {
    return Math.floor(v)===v;
  };
  this.isNull = function (v) {
    return v === null;
  };
  this.isRecord = function (v) {
    return (typeof(v)=="function") && (typeof(v.$create) == "function");
  };
  this.isUndefined = function (v) {
    return v == undefined;
  };
  this.isDefined = function (v) {
    return !(v == undefined);
  };
  this.isUTF16Char = function (v) {
    if (typeof(v)!="string") return false;
    if ((v.length==0) || (v.length>2)) return false;
    var code = v.charCodeAt(0);
    if (code < 0xD800){
      if (v.length == 1) return true;
    } else if (code <= 0xDBFF){
      if (v.length==2){
        code = v.charCodeAt(1);
        if (code >= 0xDC00 && code <= 0xDFFF) return true;
      };
    };
    return false;
  };
  this.jsInstanceOf = function (aFunction, aFunctionWithPrototype) {
    return aFunction instanceof aFunctionWithPrototype;
  };
  this.toNumber = function (v) {
    return v-0;
  };
  this.toInteger = function (v) {
    var Result = 0;
    if ($mod.isInteger(v)) {
      Result = Math.floor(v)}
     else Result = 0;
    return Result;
  };
  this.toObject = function (Value) {
    var Result = null;
    if (rtl.isObject(Value)) {
      Result = rtl.getObject(Value)}
     else Result = null;
    return Result;
  };
  this.toArray = function (Value) {
    var Result = null;
    if (rtl.isArray(Value)) {
      Result = rtl.getObject(Value)}
     else Result = null;
    return Result;
  };
  this.toBoolean = function (Value) {
    var Result = false;
    if ($mod.isBoolean(Value)) {
      Result = !(Value == false)}
     else Result = false;
    return Result;
  };
  this.ToString = function (Value) {
    var Result = "";
    if (rtl.isString(Value)) {
      Result = "" + Value}
     else Result = "";
    return Result;
  };
  this.TJSValueType = {"0": "jvtNull", jvtNull: 0, "1": "jvtBoolean", jvtBoolean: 1, "2": "jvtInteger", jvtInteger: 2, "3": "jvtFloat", jvtFloat: 3, "4": "jvtString", jvtString: 4, "5": "jvtObject", jvtObject: 5, "6": "jvtArray", jvtArray: 6};
  this.GetValueType = function (JS) {
    var Result = 0;
    var t = "";
    if ($mod.isNull(JS)) {
      Result = $mod.TJSValueType.jvtNull}
     else {
      t = typeof(JS);
      if (t === "string") {
        Result = $mod.TJSValueType.jvtString}
       else if (t === "boolean") {
        Result = $mod.TJSValueType.jvtBoolean}
       else if (t === "object") {
        if (rtl.isArray(JS)) {
          Result = $mod.TJSValueType.jvtArray}
         else Result = $mod.TJSValueType.jvtObject;
      } else if (t === "number") if ($mod.isInteger(JS)) {
        Result = $mod.TJSValueType.jvtInteger}
       else Result = $mod.TJSValueType.jvtFloat;
    };
    return Result;
  };
});
rtl.module("SysUtils",["System","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.FreeAndNil = function (Obj) {
    var o = null;
    o = Obj.get();
    if (o === null) return;
    Obj.set(null);
    o.$destroy("Destroy");
  };
  this.TFloatRec = function (s) {
    if (s) {
      this.Exponent = s.Exponent;
      this.Negative = s.Negative;
      this.Digits = s.Digits;
    } else {
      this.Exponent = 0;
      this.Negative = false;
      this.Digits = [];
    };
    this.$equal = function (b) {
      return (this.Exponent === b.Exponent) && ((this.Negative === b.Negative) && (this.Digits === b.Digits));
    };
  };
  this.TEndian = {"0": "Little", Little: 0, "1": "Big", Big: 1};
  rtl.createClass($mod,"Exception",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fMessage = "";
      this.fHelpContext = 0;
    };
    this.Create$1 = function (Msg) {
      this.fMessage = Msg;
    };
    this.CreateFmt = function (Msg, Args) {
      this.fMessage = $mod.Format(Msg,Args);
    };
    this.CreateHelp = function (Msg, AHelpContext) {
      this.fMessage = Msg;
      this.fHelpContext = AHelpContext;
    };
    this.CreateFmtHelp = function (Msg, Args, AHelpContext) {
      this.fMessage = $mod.Format(Msg,Args);
      this.fHelpContext = AHelpContext;
    };
    this.ToString = function () {
      var Result = "";
      Result = (this.$classname + ": ") + this.fMessage;
      return Result;
    };
  });
  rtl.createClass($mod,"EExternal",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EMathError",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EInvalidOp",$mod.EMathError,function () {
  });
  rtl.createClass($mod,"EZeroDivide",$mod.EMathError,function () {
  });
  rtl.createClass($mod,"EOverflow",$mod.EMathError,function () {
  });
  rtl.createClass($mod,"EUnderflow",$mod.EMathError,function () {
  });
  rtl.createClass($mod,"EAbort",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EInvalidCast",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EAssertionFailed",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EObjectCheck",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EConvertError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EFormatError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EIntError",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EDivByZero",$mod.EIntError,function () {
  });
  rtl.createClass($mod,"ERangeError",$mod.EIntError,function () {
  });
  rtl.createClass($mod,"EIntOverflow",$mod.EIntError,function () {
  });
  rtl.createClass($mod,"EInOutError",$mod.Exception,function () {
    this.$init = function () {
      $mod.Exception.$init.call(this);
      this.ErrorCode = 0;
    };
  });
  rtl.createClass($mod,"EHeapMemoryError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EExternalException",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EInvalidPointer",$mod.EHeapMemoryError,function () {
  });
  rtl.createClass($mod,"EOutOfMemory",$mod.EHeapMemoryError,function () {
  });
  rtl.createClass($mod,"EVariantError",$mod.Exception,function () {
    this.$init = function () {
      $mod.Exception.$init.call(this);
      this.ErrCode = 0;
    };
    this.CreateCode = function (Code) {
      this.ErrCode = Code;
    };
  });
  rtl.createClass($mod,"EAccessViolation",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EBusError",$mod.EAccessViolation,function () {
  });
  rtl.createClass($mod,"EPrivilege",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EStackOverflow",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EControlC",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EAbstractError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EPropReadOnly",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EPropWriteOnly",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EIntfCastError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EInvalidContainer",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EInvalidInsert",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EPackageError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EOSError",$mod.Exception,function () {
    this.$init = function () {
      $mod.Exception.$init.call(this);
      this.ErrorCode = 0;
    };
  });
  rtl.createClass($mod,"ESafecallException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"ENoThreadSupport",$mod.Exception,function () {
  });
  rtl.createClass($mod,"ENoWideStringSupport",$mod.Exception,function () {
  });
  rtl.createClass($mod,"ENotImplemented",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EArgumentException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EArgumentOutOfRangeException",$mod.EArgumentException,function () {
  });
  rtl.createClass($mod,"EArgumentNilException",$mod.EArgumentException,function () {
  });
  rtl.createClass($mod,"EPathTooLongException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"ENotSupportedException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EDirectoryNotFoundException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EFileNotFoundException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EPathNotFoundException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"ENoConstructException",$mod.Exception,function () {
  });
  this.EmptyStr = "";
  this.EmptyWideStr = "";
  this.HexDisplayPrefix = "$";
  this.LeadBytes = {};
  this.CharInSet = function (Ch, CSet) {
    var Result = false;
    var I = 0;
    Result = false;
    I = rtl.length(CSet) - 1;
    while (!Result && (I >= 0)) {
      Result = Ch === CSet[I];
      I -= 1;
    };
    return Result;
  };
  this.LeftStr = function (S, Count) {
    return (Count>0) ? S.substr(0,Count) : "";
  };
  this.RightStr = function (S, Count) {
    var l = S.length;
    return (Count<1) ? "" : ( Count>=l ? S : S.substr(l-Count));
  };
  this.Trim = function (S) {
    return S.trim();
  };
  this.TrimLeft = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
  };
  this.TrimRight = function (S) {
    return S.replace(/[\s\uFEFF\xA0\x00-\x1f]+$/,'');
  };
  this.UpperCase = function (s) {
    return s.toUpperCase();
  };
  this.LowerCase = function (s) {
    return s.toLowerCase();
  };
  this.CompareStr = function (s1, s2) {
    var l1 = s1.length;
    var l2 = s2.length;
    if (l1<=l2){
      var s = s2.substr(0,l1);
      if (s1<s){ return -1;
      } else if (s1>s){ return 1;
      } else { return l1<l2 ? -1 : 0; };
    } else {
      var s = s1.substr(0,l2);
      if (s<s2){ return -1;
      } else { return 1; };
    };
  };
  this.SameStr = function (s1, s2) {
    return s1 == s2;
  };
  this.CompareText = function (s1, s2) {
    var l1 = s1.toLowerCase();
    var l2 = s2.toLowerCase();
    if (l1>l2){ return 1;
    } else if (l1<l2){ return -1;
    } else { return 0; };
  };
  this.SameText = function (s1, s2) {
    return s1.toLowerCase() == s2.toLowerCase();
  };
  this.AnsiCompareText = function (s1, s2) {
    return s1.localeCompare(s2);
  };
  this.AnsiSameText = function (s1, s2) {
    return s1.localeCompare(s2) == 0;
  };
  this.AnsiCompareStr = function (s1, s2) {
    var Result = 0;
    Result = $mod.CompareText(s1,s2);
    return Result;
  };
  this.AppendStr = function (Dest, S) {
    Dest.set(Dest.get() + S);
  };
  this.Format = function (Fmt, Args) {
    var Result = "";
    var ChPos = 0;
    var OldPos = 0;
    var ArgPos = 0;
    var DoArg = 0;
    var Len = 0;
    var Hs = "";
    var ToAdd = "";
    var Index = 0;
    var Width = 0;
    var Prec = 0;
    var Left = false;
    var Fchar = "";
    var vq = 0;
    function ReadFormat() {
      var Result = "";
      var Value = 0;
      function ReadInteger() {
        var Code = 0;
        var ArgN = 0;
        if (Value !== -1) return;
        OldPos = ChPos;
        while (((ChPos <= Len) && (Fmt.charAt(ChPos - 1) <= "9")) && (Fmt.charAt(ChPos - 1) >= "0")) ChPos += 1;
        if (ChPos > Len) $impl.DoFormatError(1,Fmt);
        if (Fmt.charAt(ChPos - 1) === "*") {
          if (Index === -1) {
            ArgN = ArgPos}
           else {
            ArgN = Index;
            Index += 1;
          };
          if ((ChPos > OldPos) || (ArgN > (rtl.length(Args) - 1))) $impl.DoFormatError(1,Fmt);
          ArgPos = ArgN + 1;
          if (rtl.isNumber(Args[ArgN]) && pas.JS.isInteger(Args[ArgN])) {
            Value = Math.floor(Args[ArgN])}
           else $impl.DoFormatError(1,Fmt);
          ChPos += 1;
        } else {
          if (OldPos < ChPos) {
            pas.System.val(pas.System.Copy(Fmt,OldPos,ChPos - OldPos),{get: function () {
                return Value;
              }, set: function (v) {
                Value = v;
              }},{get: function () {
                return Code;
              }, set: function (v) {
                Code = v;
              }});
            if (Code > 0) $impl.DoFormatError(1,Fmt);
          } else Value = -1;
        };
      };
      function ReadIndex() {
        if (Fmt.charAt(ChPos - 1) !== ":") {
          ReadInteger()}
         else Value = 0;
        if (Fmt.charAt(ChPos - 1) === ":") {
          if (Value === -1) $impl.DoFormatError(2,Fmt);
          Index = Value;
          Value = -1;
          ChPos += 1;
        };
      };
      function ReadLeft() {
        if (Fmt.charAt(ChPos - 1) === "-") {
          Left = true;
          ChPos += 1;
        } else Left = false;
      };
      function ReadWidth() {
        ReadInteger();
        if (Value !== -1) {
          Width = Value;
          Value = -1;
        };
      };
      function ReadPrec() {
        if (Fmt.charAt(ChPos - 1) === ".") {
          ChPos += 1;
          ReadInteger();
          if (Value === -1) Value = 0;
          Prec = Value;
        };
      };
      Index = -1;
      Width = -1;
      Prec = -1;
      Value = -1;
      ChPos += 1;
      if (Fmt.charAt(ChPos - 1) === "%") {
        Result = "%";
        return Result;
      };
      ReadIndex();
      ReadLeft();
      ReadWidth();
      ReadPrec();
      Result = pas.System.upcase(Fmt.charAt(ChPos - 1));
      return Result;
    };
    function Checkarg(AT, err) {
      var Result = false;
      Result = false;
      if (Index === -1) {
        DoArg = ArgPos}
       else DoArg = Index;
      ArgPos = DoArg + 1;
      if ((DoArg > (rtl.length(Args) - 1)) || (pas.JS.GetValueType(Args[DoArg]) !== AT)) {
        if (err) $impl.DoFormatError(3,Fmt);
        ArgPos -= 1;
        return Result;
      };
      Result = true;
      return Result;
    };
    Result = "";
    Len = Fmt.length;
    ChPos = 1;
    OldPos = 1;
    ArgPos = 0;
    while (ChPos <= Len) {
      while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) !== "%")) ChPos += 1;
      if (ChPos > OldPos) Result = Result + pas.System.Copy(Fmt,OldPos,ChPos - OldPos);
      if (ChPos < Len) {
        Fchar = ReadFormat();
        var $tmp1 = Fchar;
        if ($tmp1 === "D") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          ToAdd = $mod.IntToStr(Math.floor(Args[DoArg]));
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          if (ToAdd.charAt(0) !== "-") {
            ToAdd = pas.System.StringOfChar("0",Index) + ToAdd}
           else pas.System.Insert(pas.System.StringOfChar("0",Index + 1),{get: function () {
              return ToAdd;
            }, set: function (v) {
              ToAdd = v;
            }},2);
        } else if ($tmp1 === "U") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          if (Math.floor(Args[DoArg]) < 0) $impl.DoFormatError(3,Fmt);
          ToAdd = $mod.IntToStr(Math.floor(Args[DoArg]));
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          ToAdd = pas.System.StringOfChar("0",Index) + ToAdd;
        } else if ($tmp1 === "E") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffFixed,9999,Prec);
        } else if ($tmp1 === "F") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffFixed,9999,Prec);
        } else if ($tmp1 === "G") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffGeneral,Prec,3);
        } else if ($tmp1 === "N") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffNumber,9999,Prec);
        } else if ($tmp1 === "M") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffNumber,9999,Prec);
        } else if ($tmp1 === "S") {
          Checkarg(pas.JS.TJSValueType.jvtString,true);
          Hs = "" + Args[DoArg];
          Index = Hs.length;
          if ((Prec !== -1) && (Index > Prec)) Index = Prec;
          ToAdd = pas.System.Copy(Hs,1,Index);
        } else if ($tmp1 === "P") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          ToAdd = $mod.IntToHex(Math.floor(Args[DoArg]),31);
        } else if ($tmp1 === "X") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          vq = Math.floor(Args[DoArg]);
          Index = 31;
          if (Prec > Index) {
            ToAdd = $mod.IntToHex(vq,Index)}
           else {
            Index = 1;
            while (((1 << (Index * 4)) <= vq) && (Index < 16)) Index += 1;
            if (Index > Prec) Prec = Index;
            ToAdd = $mod.IntToHex(vq,Prec);
          };
        } else if ($tmp1 === "%") ToAdd = "%";
        if (Width !== -1) if (ToAdd.length < Width) if (!Left) {
          ToAdd = pas.System.StringOfChar(" ",Width - ToAdd.length) + ToAdd}
         else ToAdd = ToAdd + pas.System.StringOfChar(" ",Width - ToAdd.length);
        Result = Result + ToAdd;
      };
      ChPos += 1;
      OldPos = ChPos;
    };
    return Result;
  };
  this.LocaleCompare = function (s1, s2, locales) {
    return s1.localeCompare(s2,locales) == 0;
  };
  this.NormalizeStr = function (S, Norm) {
    return S.normalize(Norm);
  };
  var Alpha = rtl.createSet(null,65,90,null,97,122,95);
  var AlphaNum = rtl.unionSet(Alpha,rtl.createSet(null,48,57));
  var Dot = ".";
  this.IsValidIdent = function (Ident, AllowDots, StrictDots) {
    var Result = false;
    var First = false;
    var I = 0;
    var Len = 0;
    Len = Ident.length;
    if (Len < 1) return false;
    First = true;
    Result = false;
    I = 1;
    while (I <= Len) {
      if (First) {
        if (!(Ident.charCodeAt(I - 1) in Alpha)) return Result;
        First = false;
      } else if (AllowDots && (Ident.charAt(I - 1) === Dot)) {
        if (StrictDots) {
          if (I >= Len) return Result;
          First = true;
        };
      } else if (!(Ident.charCodeAt(I - 1) in AlphaNum)) return Result;
      I = I + 1;
    };
    Result = true;
    return Result;
  };
  this.TStringReplaceFlag = {"0": "rfReplaceAll", rfReplaceAll: 0, "1": "rfIgnoreCase", rfIgnoreCase: 1};
  this.StringReplace = function (aOriginal, aSearch, aReplace, Flags) {
    var Result = "";
    var REFlags = "";
    var REString = "";
    REFlags = "";
    if ($mod.TStringReplaceFlag.rfReplaceAll in Flags) REFlags = "g";
    if ($mod.TStringReplaceFlag.rfIgnoreCase in Flags) REFlags = REFlags + "i";
    REString = aSearch.replace(new RegExp($impl.RESpecials,"g"),"\\$1");
    Result = aOriginal.replace(new RegExp(REString,REFlags),aReplace);
    return Result;
  };
  this.QuoteString = function (aOriginal, AQuote) {
    var Result = "";
    var REString = "";
    REString = AQuote.replace(new RegExp(aOriginal,"g"),"\\\\$1");
    Result = (AQuote + aOriginal.replace(new RegExp(REString,"g"),"$1\\$1")) + AQuote;
    return Result;
  };
  this.IsDelimiter = function (Delimiters, S, Index) {
    var Result = false;
    Result = false;
    if ((Index > 0) && (Index <= S.length)) Result = pas.System.Pos(S.charAt(Index - 1),Delimiters) !== 0;
    return Result;
  };
  this.AdjustLineBreaks = function (S) {
    var Result = "";
    Result = $mod.AdjustLineBreaks$1(S,pas.System.DefaultTextLineBreakStyle);
    return Result;
  };
  this.AdjustLineBreaks$1 = function (S, Style) {
    var Result = "";
    var I = 0;
    var L = 0;
    var Res = "";
    function Add(C) {
      Res = Res + C;
    };
    I = 0;
    L = S.length;
    Result = "";
    while (I <= L) {
      var $tmp1 = S.charAt(I - 1);
      if ($tmp1 === "\n") {
        if (Style in rtl.createSet(pas.System.TTextLineBreakStyle.tlbsCRLF,pas.System.TTextLineBreakStyle.tlbsCR)) Add("\r");
        if (Style === pas.System.TTextLineBreakStyle.tlbsCRLF) Add("\n");
        I += 1;
      } else if ($tmp1 === "\r") {
        if (Style === pas.System.TTextLineBreakStyle.tlbsCRLF) Add("\r");
        Add("\n");
        I += 1;
        if (S.charAt(I - 1) === "\n") I += 1;
      } else {
        Add(S.charAt(I - 1));
        I += 1;
      };
    };
    Result = Res;
    return Result;
  };
  var Quotes = rtl.createSet(39,34);
  this.WrapText = function (Line, BreakStr, BreakChars, MaxCol) {
    var Result = "";
    var L = "";
    var C = "";
    var LQ = "";
    var BC = "";
    var P = 0;
    var BLen = 0;
    var Len = 0;
    var HB = false;
    var IBC = false;
    Result = "";
    L = Line;
    BLen = BreakStr.length;
    if (BLen > 0) {
      BC = BreakStr.charAt(0)}
     else BC = "\x00";
    Len = L.length;
    while (Len > 0) {
      P = 1;
      LQ = "\x00";
      HB = false;
      IBC = false;
      while (((P <= Len) && ((P <= MaxCol) || !IBC)) && ((LQ !== "\x00") || !HB)) {
        C = L.charAt(P - 1);
        if (C === LQ) {
          LQ = "\x00"}
         else if (C.charCodeAt() in Quotes) LQ = C;
        if (LQ !== "\x00") {
          P += 1}
         else {
          HB = (C === BC) && (BreakStr === pas.System.Copy(L,P,BLen));
          if (HB) {
            P += BLen}
           else {
            if (P >= MaxCol) IBC = $mod.CharInSet(C,BreakChars);
            P += 1;
          };
        };
      };
      Result = Result + pas.System.Copy(L,1,P - 1);
      pas.System.Delete({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }},1,P - 1);
      Len = L.length;
      if ((Len > 0) && !HB) Result = Result + BreakStr;
    };
    return Result;
  };
  this.WrapText$1 = function (Line, MaxCol) {
    var Result = "";
    Result = $mod.WrapText(Line,pas.System.sLineBreak,[" ","-","\t"],MaxCol);
    return Result;
  };
  this.IntToStr = function (Value) {
    var Result = "";
    Result = "" + Value;
    return Result;
  };
  this.TryStrToInt = function (S, res) {
    var Result = false;
    var NI = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return NI;
      }, set: function (v) {
        NI = v;
      }});
    if (Result) res.set(NI);
    return Result;
  };
  this.TryStrToInt$1 = function (S, res) {
    var Result = false;
    var Radix = 10;
    var F = "";
    var N = "";
    var J = undefined;
    N = S;
    F = pas.System.Copy(N,1,1);
    if (F === "$") {
      Radix = 16}
     else if (F === "&") {
      Radix = 8}
     else if (F === "%") Radix = 2;
    if (Radix !== 10) pas.System.Delete({get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }},1,1);
    J = parseInt(N,Radix);
    Result = !isNaN(J);
    if (Result) res.set(Math.floor(J));
    return Result;
  };
  this.StrToIntDef = function (S, aDef) {
    var Result = 0;
    var R = 0;
    if ($mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) {
      Result = R}
     else Result = aDef;
    return Result;
  };
  this.StrToIntDef$1 = function (S, aDef) {
    var Result = 0;
    var R = 0;
    if ($mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) {
      Result = R}
     else Result = aDef;
    return Result;
  };
  this.StrToInt = function (S) {
    var Result = 0;
    var R = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    Result = R;
    return Result;
  };
  this.StrToNativeInt = function (S) {
    var Result = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    return Result;
  };
  this.StrToInt64 = function (S) {
    var Result = 0;
    var N = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    Result = N;
    return Result;
  };
  this.StrToInt64Def = function (S, ADefault) {
    var Result = 0;
    if ($mod.TryStrToInt64(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = ADefault;
    return Result;
  };
  this.TryStrToInt64 = function (S, res) {
    var Result = false;
    var R = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }});
    if (Result) res.set(R);
    return Result;
  };
  this.StrToQWord = function (S) {
    var Result = 0;
    var N = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }}) || (N < 0)) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    Result = N;
    return Result;
  };
  this.StrToQWordDef = function (S, ADefault) {
    var Result = 0;
    if (!$mod.TryStrToQWord(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = ADefault;
    return Result;
  };
  this.TryStrToQWord = function (S, res) {
    var Result = false;
    var R = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }}) && (R >= 0);
    if (Result) res.set(R);
    return Result;
  };
  this.StrToUInt64 = function (S) {
    var Result = 0;
    var N = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }}) || (N < 0)) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    Result = N;
    return Result;
  };
  this.StrToUInt64Def = function (S, ADefault) {
    var Result = 0;
    if (!$mod.TryStrToUInt64(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = ADefault;
    return Result;
  };
  this.TryStrToUInt64 = function (S, res) {
    var Result = false;
    var R = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }}) && (R >= 0);
    if (Result) res.set(R);
    return Result;
  };
  this.StrToDWord = function (S) {
    var Result = 0;
    if (!$mod.TryStrToDWord(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    return Result;
  };
  this.StrToDWordDef = function (S, ADefault) {
    var Result = 0;
    if (!$mod.TryStrToDWord(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = ADefault;
    return Result;
  };
  this.TryStrToDWord = function (S, res) {
    var Result = false;
    var R = 0;
    Result = ($mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }}) && (R >= 0)) && (R <= 0xFFFFFFFF);
    if (Result) res.set(R);
    return Result;
  };
  var HexDigits = "0123456789ABCDEF";
  this.IntToHex = function (Value, Digits) {
    var Result = "";
    if (Digits === 0) Digits = 1;
    Result = "";
    while (Value > 0) {
      Result = HexDigits.charAt(((Value & 15) + 1) - 1) + Result;
      Value = Value >>> 4;
    };
    while (Result.length < Digits) Result = "0" + Result;
    return Result;
  };
  this.MaxCurrency = 922337203685477.0000;
  this.MinCurrency = -922337203685477.0000;
  this.TFloatFormat = {"0": "ffFixed", ffFixed: 0, "1": "ffGeneral", ffGeneral: 1, "2": "ffExponent", ffExponent: 2, "3": "ffNumber", ffNumber: 3};
  var Rounds = "234567890";
  this.FloatToDecimal = function (Value, Precision, Decimals) {
    var Result = new $mod.TFloatRec();
    var Buffer = "";
    var InfNan = "";
    var error = 0;
    var N = 0;
    var L = 0;
    var Start = 0;
    var C = 0;
    var GotNonZeroBeforeDot = false;
    var BeforeDot = false;
    if (Value === 0) ;
    Result.Digits = rtl.arraySetLength(Result.Digits,"",19);
    Buffer=Value.toPrecision(21); // Double precision;
    N = 1;
    L = Buffer.length;
    while (Buffer.charAt(N - 1) === " ") N += 1;
    Result.Negative = Buffer.charAt(N - 1) === "-";
    if (Result.Negative) {
      N += 1}
     else if (Buffer.charAt(N - 1) === "+") N += 1;
    if (L >= (N + 2)) {
      InfNan = pas.System.Copy(Buffer,N,3);
      if (InfNan === "Inf") {
        Result.Digits[0] = "\x00";
        Result.Exponent = 32767;
        return Result;
      };
      if (InfNan === "Nan") {
        Result.Digits[0] = "\x00";
        Result.Exponent = -32768;
        return Result;
      };
    };
    Start = N;
    Result.Exponent = 0;
    BeforeDot = true;
    GotNonZeroBeforeDot = false;
    while ((L >= N) && (Buffer.charAt(N - 1) !== "E")) {
      if (Buffer.charAt(N - 1) === ".") {
        BeforeDot = false}
       else {
        if (BeforeDot) {
          Result.Exponent += 1;
          Result.Digits[N - Start] = Buffer.charAt(N - 1);
          if (Buffer.charAt(N - 1) !== "0") GotNonZeroBeforeDot = true;
        } else Result.Digits[(N - Start) - 1] = Buffer.charAt(N - 1);
      };
      N += 1;
    };
    N += 1;
    if (N <= L) {
      pas.System.val$5(pas.System.Copy(Buffer,N,(L - N) + 1),{get: function () {
          return C;
        }, set: function (v) {
          C = v;
        }},{get: function () {
          return error;
        }, set: function (v) {
          error = v;
        }});
      Result.Exponent += C;
    };
    if (BeforeDot) {
      N = (N - Start) - 1}
     else N = (N - Start) - 2;
    L = rtl.length(Result.Digits);
    if (N < L) Result.Digits[N] = "0";
    if ((Decimals + Result.Exponent) < Precision) {
      N = Decimals + Result.Exponent}
     else N = Precision;
    if (N >= L) N = L - 1;
    if (N === 0) {
      if (Result.Digits[0] >= "5") {
        Result.Digits[0] = "1";
        Result.Digits[1] = "\x00";
        Result.Exponent += 1;
      } else Result.Digits[0] = "\x00";
    } else if (N > 0) {
      if (Result.Digits[N] >= "5") {
        do {
          Result.Digits[N] = "\x00";
          N -= 1;
          Result.Digits[N] = Rounds.charAt($mod.StrToInt(Result.Digits[N]) - 1);
        } while (!((N === 0) || (Result.Digits[N] < ":")));
        if (Result.Digits[0] === ":") {
          Result.Digits[0] = "1";
          Result.Exponent += 1;
        };
      } else {
        Result.Digits[N] = "0";
        while ((N > -1) && (Result.Digits[N] === "0")) {
          Result.Digits[N] = "\x00";
          N -= 1;
        };
      };
    } else Result.Digits[0] = "\x00";
    if ((Result.Digits[0] === "\x00") && !GotNonZeroBeforeDot) {
      Result.Exponent = 0;
      Result.Negative = false;
    };
    return Result;
  };
  this.FloatToStr = function (Value) {
    var Result = "";
    Result = $mod.FloatToStrF(Value,$mod.TFloatFormat.ffGeneral,15,0);
    return Result;
  };
  this.FloatToStrF = function (Value, format, Precision, Digits) {
    var Result = "";
    var DS = "";
    function RemoveLeadingNegativeSign(AValue) {
      var Result = false;
      var i = 0;
      var TS = "";
      var StartPos = 0;
      Result = false;
      StartPos = 2;
      TS = $mod.ThousandSeparator;
      for (var $l1 = StartPos, $end2 = AValue.get().length; $l1 <= $end2; $l1++) {
        i = $l1;
        Result = (AValue.get().charCodeAt(i - 1) in rtl.createSet(48,DS.charCodeAt(),69,43)) || (AValue.get() === TS);
        if (!Result) break;
      };
      if (Result) pas.System.Delete(AValue,1,1);
      return Result;
    };
    DS = $mod.DecimalSeparator;
    var $tmp1 = format;
    if ($tmp1 === $mod.TFloatFormat.ffGeneral) {
      Result = $impl.FormatGeneralFloat(Value,Precision,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffExponent) {
      Result = $impl.FormatExponentFloat(Value,Precision,Digits,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffFixed) {
      Result = $impl.FormatFixedFloat(Value,Digits,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffNumber) Result = $impl.FormatNumberFloat(Value,Digits,DS,$mod.ThousandSeparator);
    if ((Result.length > 1) && (Result.charAt(0) === "-")) RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }});
    return Result;
  };
  this.TryStrToFloat = function (S, res) {
    var Result = false;
    var J = undefined;
    var N = "";
    N = S;
    if ($mod.ThousandSeparator !== "") N = $mod.StringReplace(N,$mod.ThousandSeparator,"",rtl.createSet($mod.TStringReplaceFlag.rfReplaceAll));
    if ($mod.DecimalSeparator !== ".") N = $mod.StringReplace(N,$mod.DecimalSeparator,".",{});
    J = parseFloat(N);
    Result = !isNaN(J);
    if (Result) res.set(rtl.getNumber(J));
    return Result;
  };
  this.StrToFloatDef = function (S, aDef) {
    var Result = 0.0;
    if (!$mod.TryStrToFloat(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = aDef;
    return Result;
  };
  this.StrToFloat = function (S) {
    var Result = 0.0;
    if (!$mod.TryStrToFloat(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidFloat,[S]]);
    return Result;
  };
  var MaxPrecision = 18;
  this.FormatFloat = function (Fmt, aValue) {
    var Result = "";
    var E = 0.0;
    var FV = new $mod.TFloatRec();
    var Section = "";
    var SectionLength = 0;
    var ThousandSep = false;
    var IsScientific = false;
    var DecimalPos = 0;
    var FirstDigit = 0;
    var LastDigit = 0;
    var RequestedDigits = 0;
    var ExpSize = 0;
    var Available = 0;
    var Current = 0;
    var PadZeroes = 0;
    var DistToDecimal = 0;
    function InitVars() {
      E = aValue;
      Section = "";
      SectionLength = 0;
      ThousandSep = false;
      IsScientific = false;
      DecimalPos = 0;
      FirstDigit = 2147483647;
      LastDigit = 0;
      RequestedDigits = 0;
      ExpSize = 0;
      Available = -1;
    };
    function ToResult(AChar) {
      Result = Result + AChar;
    };
    function AddToResult(AStr) {
      Result = Result + AStr;
    };
    function WriteDigit(ADigit) {
      if (ADigit === "\x00") return;
      DistToDecimal -= 1;
      if (DistToDecimal === -1) {
        AddToResult($mod.DecimalSeparator);
        ToResult(ADigit);
      } else {
        ToResult(ADigit);
        if ((ThousandSep && ((DistToDecimal % 3) === 0)) && (DistToDecimal > 1)) AddToResult($mod.ThousandSeparator);
      };
    };
    function GetDigit() {
      var Result = "";
      Result = "\x00";
      if (Current <= Available) {
        Result = FV.Digits[Current];
        Current += 1;
      } else if (DistToDecimal <= LastDigit) {
        DistToDecimal -= 1}
       else Result = "0";
      return Result;
    };
    function CopyDigit() {
      if (PadZeroes === 0) {
        WriteDigit(GetDigit())}
       else if (PadZeroes < 0) {
        PadZeroes += 1;
        if (DistToDecimal <= FirstDigit) {
          WriteDigit("0")}
         else DistToDecimal -= 1;
      } else {
        while (PadZeroes > 0) {
          WriteDigit(GetDigit());
          PadZeroes -= 1;
        };
        WriteDigit(GetDigit());
      };
    };
    function GetSections(SP) {
      var Result = 0;
      var FL = 0;
      var i = 0;
      var C = "";
      var Q = "";
      var inQuote = false;
      Result = 1;
      SP.get()[1] = -1;
      SP.get()[2] = -1;
      SP.get()[3] = -1;
      inQuote = false;
      Q = "\x00";
      i = 1;
      FL = Fmt.length;
      while (i <= FL) {
        C = Fmt.charAt(i - 1);
        var $tmp1 = C;
        if ($tmp1 === ";") {
          if (!inQuote) {
            if (Result > 3) throw $mod.Exception.$create("Create$1",["Invalid float format"]);
            SP.get()[Result] = i + 1;
            Result += 1;
          };
        } else if (($tmp1 === '"') || ($tmp1 === "'")) {
          if (inQuote) {
            inQuote = C !== Q}
           else {
            inQuote = true;
            Q = C;
          };
        };
        i += 1;
      };
      if (SP.get()[Result] === -1) SP.get()[Result] = FL + 1;
      return Result;
    };
    function AnalyzeFormat() {
      var I = 0;
      var Len = 0;
      var Q = "";
      var C = "";
      var InQuote = false;
      Len = Section.length;
      I = 1;
      InQuote = false;
      Q = "\x00";
      while (I <= Len) {
        C = Section.charAt(I - 1);
        if (C.charCodeAt() in rtl.createSet(34,39)) {
          if (InQuote) {
            InQuote = C !== Q}
           else {
            InQuote = true;
            Q = C;
          };
        } else if (!InQuote) {
          var $tmp1 = C;
          if ($tmp1 === ".") {
            if (DecimalPos === 0) DecimalPos = RequestedDigits + 1}
           else if ($tmp1 === ",") {
            ThousandSep = $mod.ThousandSeparator !== "\x00"}
           else if (($tmp1 === "e") || ($tmp1 === "E")) {
            I += 1;
            if (I < Len) {
              C = Section.charAt(I - 1);
              IsScientific = C.charCodeAt() in rtl.createSet(45,43);
              if (IsScientific) while ((I < Len) && (Section.charAt((I + 1) - 1) === "0")) {
                ExpSize += 1;
                I += 1;
              };
              if (ExpSize > 4) ExpSize = 4;
            };
          } else if ($tmp1 === "#") {
            RequestedDigits += 1}
           else if ($tmp1 === "0") {
            if (RequestedDigits < FirstDigit) FirstDigit = RequestedDigits + 1;
            RequestedDigits += 1;
            LastDigit = RequestedDigits + 1;
          };
        };
        I += 1;
      };
      if (DecimalPos === 0) DecimalPos = RequestedDigits + 1;
      LastDigit = DecimalPos - LastDigit;
      if (LastDigit > 0) LastDigit = 0;
      FirstDigit = DecimalPos - FirstDigit;
      if (FirstDigit < 0) FirstDigit = 0;
    };
    function ValueOutSideScope() {
      var Result = false;
      Result = (((FV.Exponent >= 18) && !IsScientific) || (FV.Exponent === 0x7FF)) || (FV.Exponent === 0x800);
      return Result;
    };
    function CalcRunVars() {
      var D = 0;
      var P = 0;
      if (IsScientific) {
        P = RequestedDigits;
        D = 9999;
      } else {
        P = 18;
        D = (RequestedDigits - DecimalPos) + 1;
      };
      FV = new $mod.TFloatRec($mod.FloatToDecimal(aValue,P,D));
      DistToDecimal = DecimalPos - 1;
      if (IsScientific) {
        PadZeroes = 0}
       else {
        PadZeroes = FV.Exponent - (DecimalPos - 1);
        if (PadZeroes >= 0) DistToDecimal = FV.Exponent;
      };
      Available = -1;
      while ((Available < (rtl.length(FV.Digits) - 1)) && (FV.Digits[Available + 1] !== "\x00")) Available += 1;
    };
    function FormatExponent(ASign, aExponent) {
      var Result = "";
      Result = $mod.IntToStr(aExponent);
      Result = pas.System.StringOfChar("0",ExpSize - Result.length) + Result;
      if (aExponent < 0) {
        Result = "-" + Result}
       else if ((aExponent > 0) && (ASign === "+")) Result = ASign + Result;
      return Result;
    };
    var I = 0;
    var S = 0;
    var C = "";
    var Q = "";
    var PA = [];
    var InLiteral = false;
    PA = rtl.arraySetLength(PA,0,4);
    Result = "";
    InitVars();
    if (E > 0) {
      S = 1}
     else if (E < 0) {
      S = 2}
     else S = 3;
    PA[0] = 0;
    I = GetSections({get: function () {
        return PA;
      }, set: function (v) {
        PA = v;
      }});
    if ((I < S) || ((PA[S] - PA[S - 1]) === 0)) S = 1;
    SectionLength = (PA[S] - PA[S - 1]) - 1;
    Section = pas.System.Copy(Fmt,PA[S - 1] + 1,SectionLength);
    Section = rtl.strSetLength(Section,SectionLength);
    AnalyzeFormat();
    CalcRunVars();
    if ((SectionLength === 0) || ValueOutSideScope()) {
      Section=E.toPrecision(15);
      Result = Section;
    };
    I = 1;
    Current = 0;
    Q = " ";
    InLiteral = false;
    if (FV.Negative && (S === 1)) ToResult("-");
    while (I <= SectionLength) {
      C = Section.charAt(I - 1);
      if (C.charCodeAt() in rtl.createSet(34,39)) {
        if (InLiteral) {
          InLiteral = C !== Q}
         else {
          InLiteral = true;
          Q = C;
        };
      } else if (InLiteral) {
        ToResult(C)}
       else {
        var $tmp1 = C;
        if (($tmp1 === "0") || ($tmp1 === "#")) {
          CopyDigit()}
         else if (($tmp1 === ".") || ($tmp1 === ",")) {}
        else if (($tmp1 === "e") || ($tmp1 === "E")) {
          ToResult(C);
          I += 1;
          if (I <= Section.length) {
            C = Section.charAt(I - 1);
            if (C.charCodeAt() in rtl.createSet(43,45)) {
              AddToResult(FormatExponent(C,(FV.Exponent - DecimalPos) + 1));
              while ((I < SectionLength) && (Section.charAt((I + 1) - 1) === "0")) I += 1;
            };
          };
        } else {
          ToResult(C);
        };
      };
      I += 1;
    };
    return Result;
  };
  this.TrueBoolStrs = [];
  this.FalseBoolStrs = [];
  this.StrToBool = function (S) {
    var Result = false;
    if (!$mod.TryStrToBool(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidBoolean,[S]]);
    return Result;
  };
  this.BoolToStr = function (B, UseBoolStrs) {
    var Result = "";
    if (UseBoolStrs) {
      $impl.CheckBoolStrs();
      if (B) {
        Result = $mod.TrueBoolStrs[0]}
       else Result = $mod.FalseBoolStrs[0];
    } else if (B) {
      Result = "-1"}
     else Result = "0";
    return Result;
  };
  this.BoolToStr$1 = function (B, TrueS, FalseS) {
    var Result = "";
    if (B) {
      Result = TrueS}
     else Result = FalseS;
    return Result;
  };
  this.StrToBoolDef = function (S, Default) {
    var Result = false;
    if (!$mod.TryStrToBool(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = Default;
    return Result;
  };
  this.TryStrToBool = function (S, Value) {
    var Result = false;
    var Temp = "";
    var I = 0;
    var D = 0.0;
    var Code = 0;
    Temp = $mod.UpperCase(S);
    pas.System.val$7(Temp,{get: function () {
        return D;
      }, set: function (v) {
        D = v;
      }},{get: function () {
        return Code;
      }, set: function (v) {
        Code = v;
      }});
    Result = true;
    if (Code === 0) {
      Value.set(D !== 0.0)}
     else {
      $impl.CheckBoolStrs();
      for (var $l1 = 0, $end2 = rtl.length($mod.TrueBoolStrs) - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        if (Temp === $mod.UpperCase($mod.TrueBoolStrs[I])) {
          Value.set(true);
          return Result;
        };
      };
      for (var $l3 = 0, $end4 = rtl.length($mod.FalseBoolStrs) - 1; $l3 <= $end4; $l3++) {
        I = $l3;
        if (Temp === $mod.UpperCase($mod.FalseBoolStrs[I])) {
          Value.set(false);
          return Result;
        };
      };
      Result = false;
    };
    return Result;
  };
  this.ConfigExtension = ".cfg";
  this.SysConfigDir = "";
  this.OnGetEnvironmentVariable = null;
  this.OnGetEnvironmentString = null;
  this.OnGetEnvironmentVariableCount = null;
  this.GetEnvironmentVariable = function (EnvVar) {
    var Result = "";
    if ($mod.OnGetEnvironmentVariable != null) {
      Result = $mod.OnGetEnvironmentVariable(EnvVar)}
     else Result = "";
    return Result;
  };
  this.GetEnvironmentVariableCount = function () {
    var Result = 0;
    if ($mod.OnGetEnvironmentVariableCount != null) {
      Result = $mod.OnGetEnvironmentVariableCount()}
     else Result = 0;
    return Result;
  };
  this.GetEnvironmentString = function (Index) {
    var Result = "";
    if ($mod.OnGetEnvironmentString != null) {
      Result = $mod.OnGetEnvironmentString(Index)}
     else Result = "";
    return Result;
  };
  this.ShowException = function (ExceptObject, ExceptAddr) {
    var S = "";
    S = "Application raised an exception " + ExceptObject.$classname;
    if ($mod.Exception.isPrototypeOf(ExceptObject)) S = (S + " : ") + ExceptObject.fMessage;
    window.alert(S);
    if (ExceptAddr === null) ;
  };
  this.Abort = function () {
    throw $mod.EAbort.$create("Create$1",[$impl.SAbortError]);
  };
  this.TEventType = {"0": "etCustom", etCustom: 0, "1": "etInfo", etInfo: 1, "2": "etWarning", etWarning: 2, "3": "etError", etError: 3, "4": "etDebug", etDebug: 4};
  this.TSystemTime = function (s) {
    if (s) {
      this.Year = s.Year;
      this.Month = s.Month;
      this.Day = s.Day;
      this.DayOfWeek = s.DayOfWeek;
      this.Hour = s.Hour;
      this.Minute = s.Minute;
      this.Second = s.Second;
      this.MilliSecond = s.MilliSecond;
    } else {
      this.Year = 0;
      this.Month = 0;
      this.Day = 0;
      this.DayOfWeek = 0;
      this.Hour = 0;
      this.Minute = 0;
      this.Second = 0;
      this.MilliSecond = 0;
    };
    this.$equal = function (b) {
      return (this.Year === b.Year) && ((this.Month === b.Month) && ((this.Day === b.Day) && ((this.DayOfWeek === b.DayOfWeek) && ((this.Hour === b.Hour) && ((this.Minute === b.Minute) && ((this.Second === b.Second) && (this.MilliSecond === b.MilliSecond)))))));
    };
  };
  this.TTimeStamp = function (s) {
    if (s) {
      this.Time = s.Time;
      this.date = s.date;
    } else {
      this.Time = 0;
      this.date = 0;
    };
    this.$equal = function (b) {
      return (this.Time === b.Time) && (this.date === b.date);
    };
  };
  this.TimeSeparator = ":";
  this.DateSeparator = "-";
  this.ShortDateFormat = "yyyy-mm-dd";
  this.LongDateFormat = "ddd, yyyy-mm-dd";
  this.ShortTimeFormat = "hh:nn";
  this.LongTimeFormat = "hh:nn:ss";
  this.DecimalSeparator = ".";
  this.ThousandSeparator = "";
  this.TimeAMString = "AM";
  this.TimePMString = "PM";
  this.HoursPerDay = 24;
  this.MinsPerHour = 60;
  this.SecsPerMin = 60;
  this.MSecsPerSec = 1000;
  this.MinsPerDay = 24 * 60;
  this.SecsPerDay = 1440 * 60;
  this.MSecsPerDay = 86400 * 1000;
  this.MaxDateTime = 2958465.99999999;
  this.MinDateTime = -693593.99999999;
  this.JulianEpoch = -2415018.5;
  this.UnixEpoch = -2415018.5 + 2440587.5;
  this.DateDelta = 693594;
  this.UnixDateDelta = 25569;
  this.MonthDays = [[31,28,31,30,31,30,31,31,30,31,30,31],[31,29,31,30,31,30,31,31,30,31,30,31]];
  this.ShortMonthNames = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
  this.LongMonthNames = ["January","February","March","April","May","June","July","August","September","October","November","December"];
  this.ShortDayNames = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"];
  this.LongDayNames = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"];
  rtl.createClass($mod,"TFormatSettings",pas.System.TObject,function () {
    this.GetDateSeparator = function () {
      var Result = "";
      Result = $mod.DateSeparator;
      return Result;
    };
    this.GetDecimalSeparator = function () {
      var Result = "";
      Result = $mod.DecimalSeparator;
      return Result;
    };
    this.GetLongDateFormat = function () {
      var Result = "";
      Result = $mod.LongDateFormat;
      return Result;
    };
    this.GetLongDayNames = function () {
      var Result = rtl.arraySetLength(null,"",7);
      Result = $mod.LongDayNames.slice(0);
      return Result;
    };
    this.GetLongMonthNames = function () {
      var Result = rtl.arraySetLength(null,"",12);
      Result = $mod.LongMonthNames.slice(0);
      return Result;
    };
    this.GetLongTimeFormat = function () {
      var Result = "";
      Result = $mod.LongTimeFormat;
      return Result;
    };
    this.GetShortDateFormat = function () {
      var Result = "";
      Result = $mod.ShortDateFormat;
      return Result;
    };
    this.GetShortDayNames = function () {
      var Result = rtl.arraySetLength(null,"",7);
      Result = $mod.ShortDayNames.slice(0);
      return Result;
    };
    this.GetShortMonthNames = function () {
      var Result = rtl.arraySetLength(null,"",12);
      Result = $mod.ShortMonthNames.slice(0);
      return Result;
    };
    this.GetShortTimeFormat = function () {
      var Result = "";
      Result = $mod.ShortTimeFormat;
      return Result;
    };
    this.GetThousandSeparator = function () {
      var Result = "";
      Result = $mod.ThousandSeparator;
      return Result;
    };
    this.GetTimeAMString = function () {
      var Result = "";
      Result = $mod.TimeAMString;
      return Result;
    };
    this.GetTimePMString = function () {
      var Result = "";
      Result = $mod.TimePMString;
      return Result;
    };
    this.GetTimeSeparator = function () {
      var Result = "";
      Result = $mod.TimeSeparator;
      return Result;
    };
    this.SetDateSeparator = function (Value) {
      $mod.DateSeparator = Value;
    };
    this.SetDecimalSeparator = function (Value) {
      $mod.DecimalSeparator = Value;
    };
    this.SetLongDateFormat = function (Value) {
      $mod.LongDateFormat = Value;
    };
    this.SetLongDayNames = function (AValue) {
      $mod.LongDayNames = AValue.slice(0);
    };
    this.SetLongMonthNames = function (AValue) {
      $mod.LongMonthNames = AValue.slice(0);
    };
    this.SetLongTimeFormat = function (Value) {
      $mod.LongTimeFormat = Value;
    };
    this.SetShortDateFormat = function (Value) {
      $mod.ShortDateFormat = Value;
    };
    this.SetShortDayNames = function (AValue) {
      $mod.ShortDayNames = AValue.slice(0);
    };
    this.SetShortMonthNames = function (AValue) {
      $mod.ShortMonthNames = AValue.slice(0);
    };
    this.SetShortTimeFormat = function (Value) {
      $mod.ShortTimeFormat = Value;
    };
    this.SetThousandSeparator = function (Value) {
      $mod.ThousandSeparator = Value;
    };
    this.SetTimeAMString = function (Value) {
      $mod.TimeAMString = Value;
    };
    this.SetTimePMString = function (Value) {
      $mod.TimePMString = Value;
    };
    this.SetTimeSeparator = function (Value) {
      $mod.TimeSeparator = Value;
    };
  });
  this.FormatSettings = null;
  this.TwoDigitYearCenturyWindow = 50;
  this.DateTimeToJSDate = function (aDateTime) {
    var Result = null;
    var Y = 0;
    var M = 0;
    var D = 0;
    var h = 0;
    var n = 0;
    var s = 0;
    var z = 0;
    $mod.DecodeDate(pas.System.Trunc(aDateTime),{get: function () {
        return Y;
      }, set: function (v) {
        Y = v;
      }},{get: function () {
        return M;
      }, set: function (v) {
        M = v;
      }},{get: function () {
        return D;
      }, set: function (v) {
        D = v;
      }});
    $mod.DecodeTime(pas.System.Frac(aDateTime),{get: function () {
        return h;
      }, set: function (v) {
        h = v;
      }},{get: function () {
        return n;
      }, set: function (v) {
        n = v;
      }},{get: function () {
        return s;
      }, set: function (v) {
        s = v;
      }},{get: function () {
        return z;
      }, set: function (v) {
        z = v;
      }});
    Result = new Date(Y,M,D,h,n,s,z);
    return Result;
  };
  this.JSDateToDateTime = function (aDate) {
    var Result = 0.0;
    Result = $mod.EncodeDate(aDate.getFullYear(),aDate.getMonth() + 1,aDate.getDate()) + $mod.EncodeTime(aDate.getHours(),aDate.getMinutes(),aDate.getSeconds(),aDate.getMilliseconds());
    return Result;
  };
  this.DateTimeToTimeStamp = function (DateTime) {
    var Result = new $mod.TTimeStamp();
    var D = 0.0;
    D = DateTime * 86400000;
    if (D < 0) {
      D = D - 0.5}
     else D = D + 0.5;
    Result.Time = pas.System.Trunc(Math.abs(pas.System.Trunc(D)) % 86400000);
    Result.date = 693594 + Math.floor(pas.System.Trunc(D) / 86400000);
    return Result;
  };
  this.TimeStampToDateTime = function (TimeStamp) {
    var Result = 0.0;
    Result = $mod.ComposeDateTime(TimeStamp.date - 693594,TimeStamp.Time / 86400000);
    return Result;
  };
  this.MSecsToTimeStamp = function (MSecs) {
    var Result = new $mod.TTimeStamp();
    Result.date = pas.System.Trunc(MSecs / 86400000);
    MSecs = MSecs - (Result.date * 86400000);
    Result.Time = Math.round(MSecs);
    return Result;
  };
  this.TimeStampToMSecs = function (TimeStamp) {
    var Result = 0;
    Result = TimeStamp.Time + (TimeStamp.date * 86400000);
    return Result;
  };
  this.TryEncodeDate = function (Year, Month, Day, date) {
    var Result = false;
    var c = 0;
    var ya = 0;
    Result = (((((Year > 0) && (Year < 10000)) && (Month >= 1)) && (Month <= 12)) && (Day > 0)) && (Day <= $mod.MonthDays[+$mod.IsLeapYear(Year)][Month - 1]);
    if (Result) {
      if (Month > 2) {
        Month -= 3}
       else {
        Month += 9;
        Year -= 1;
      };
      c = Math.floor(Year / 100);
      ya = Year - (100 * c);
      date.set(((((146097 * c) >>> 2) + ((1461 * ya) >>> 2)) + Math.floor(((153 * Month) + 2) / 5)) + Day);
      date.set(date.get() - 693900);
    };
    return Result;
  };
  this.TryEncodeTime = function (Hour, Min, Sec, MSec, Time) {
    var Result = false;
    Result = (((Hour < 24) && (Min < 60)) && (Sec < 60)) && (MSec < 1000);
    if (Result) Time.set(((((Hour * 3600000) + (Min * 60000)) + (Sec * 1000)) + MSec) / 86400000);
    return Result;
  };
  this.EncodeDate = function (Year, Month, Day) {
    var Result = 0.0;
    if (!$mod.TryEncodeDate(Year,Month,Day,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",["%s-%s-%s is not a valid date specification",[$mod.IntToStr(Year),$mod.IntToStr(Month),$mod.IntToStr(Day)]]);
    return Result;
  };
  this.EncodeTime = function (Hour, Minute, Second, MilliSecond) {
    var Result = 0.0;
    if (!$mod.TryEncodeTime(Hour,Minute,Second,MilliSecond,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",["%s:%s:%s.%s is not a valid time specification",[$mod.IntToStr(Hour),$mod.IntToStr(Minute),$mod.IntToStr(Second),$mod.IntToStr(MilliSecond)]]);
    return Result;
  };
  this.ComposeDateTime = function (date, Time) {
    var Result = 0.0;
    if (date < 0) {
      Result = pas.System.Trunc(date) - Math.abs(pas.System.Frac(Time))}
     else Result = pas.System.Trunc(date) + Math.abs(pas.System.Frac(Time));
    return Result;
  };
  this.DecodeDate = function (date, Year, Month, Day) {
    var ly = 0;
    var ld = 0;
    var lm = 0;
    var j = 0;
    if (date <= -693594) {
      Year.set(0);
      Month.set(0);
      Day.set(0);
    } else {
      if (date > 0) {
        date = date + (1 / (86400000 * 2))}
       else date = date - (1 / (86400000 * 2));
      if (date > $mod.MaxDateTime) date = $mod.MaxDateTime;
      j = ((pas.System.Trunc(date) + 693900) << 2) - 1;
      ly = Math.floor(j / 146097);
      j = j - (146097 * ly);
      ld = j >>> 2;
      j = Math.floor(((ld << 2) + 3) / 1461);
      ld = (((ld << 2) + 7) - (1461 * j)) >>> 2;
      lm = Math.floor(((5 * ld) - 3) / 153);
      ld = Math.floor((((5 * ld) + 2) - (153 * lm)) / 5);
      ly = (100 * ly) + j;
      if (lm < 10) {
        lm += 3}
       else {
        lm -= 9;
        ly += 1;
      };
      Year.set(ly);
      Month.set(lm);
      Day.set(ld);
    };
  };
  this.DecodeDateFully = function (DateTime, Year, Month, Day, DOW) {
    var Result = false;
    $mod.DecodeDate(DateTime,Year,Month,Day);
    DOW.set($mod.DayOfWeek(DateTime));
    Result = $mod.IsLeapYear(Year.get());
    return Result;
  };
  this.DecodeTime = function (Time, Hour, Minute, Second, MilliSecond) {
    var l = 0;
    l = $mod.DateTimeToTimeStamp(Time).Time;
    Hour.set(Math.floor(l / 3600000));
    l = l % 3600000;
    Minute.set(Math.floor(l / 60000));
    l = l % 60000;
    Second.set(Math.floor(l / 1000));
    l = l % 1000;
    MilliSecond.set(l);
  };
  this.DateTimeToSystemTime = function (DateTime, SystemTime) {
    $mod.DecodeDateFully(DateTime,{p: SystemTime.get(), get: function () {
        return this.p.Year;
      }, set: function (v) {
        this.p.Year = v;
      }},{p: SystemTime.get(), get: function () {
        return this.p.Month;
      }, set: function (v) {
        this.p.Month = v;
      }},{p: SystemTime.get(), get: function () {
        return this.p.Day;
      }, set: function (v) {
        this.p.Day = v;
      }},{p: SystemTime.get(), get: function () {
        return this.p.DayOfWeek;
      }, set: function (v) {
        this.p.DayOfWeek = v;
      }});
    $mod.DecodeTime(DateTime,{p: SystemTime.get(), get: function () {
        return this.p.Hour;
      }, set: function (v) {
        this.p.Hour = v;
      }},{p: SystemTime.get(), get: function () {
        return this.p.Minute;
      }, set: function (v) {
        this.p.Minute = v;
      }},{p: SystemTime.get(), get: function () {
        return this.p.Second;
      }, set: function (v) {
        this.p.Second = v;
      }},{p: SystemTime.get(), get: function () {
        return this.p.MilliSecond;
      }, set: function (v) {
        this.p.MilliSecond = v;
      }});
    SystemTime.get().DayOfWeek -= 1;
  };
  this.SystemTimeToDateTime = function (SystemTime) {
    var Result = 0.0;
    Result = $mod.ComposeDateTime($impl.DoEncodeDate(SystemTime.Year,SystemTime.Month,SystemTime.Day),$impl.DoEncodeTime(SystemTime.Hour,SystemTime.Minute,SystemTime.Second,SystemTime.MilliSecond));
    return Result;
  };
  this.DayOfWeek = function (DateTime) {
    var Result = 0;
    Result = 1 + ((pas.System.Trunc(DateTime) - 1) % 7);
    if (Result <= 0) Result += 7;
    return Result;
  };
  this.date = function () {
    var Result = 0.0;
    Result = pas.System.Trunc($mod.Now());
    return Result;
  };
  this.Time = function () {
    var Result = 0.0;
    Result = $mod.Now() - $mod.date();
    return Result;
  };
  this.Now = function () {
    var Result = 0.0;
    Result = $mod.JSDateToDateTime(new Date());
    return Result;
  };
  this.IncMonth = function (DateTime, NumberOfMonths) {
    var Result = 0.0;
    var Year = 0;
    var Month = 0;
    var Day = 0;
    $mod.DecodeDate(DateTime,{get: function () {
        return Year;
      }, set: function (v) {
        Year = v;
      }},{get: function () {
        return Month;
      }, set: function (v) {
        Month = v;
      }},{get: function () {
        return Day;
      }, set: function (v) {
        Day = v;
      }});
    $mod.IncAMonth({get: function () {
        return Year;
      }, set: function (v) {
        Year = v;
      }},{get: function () {
        return Month;
      }, set: function (v) {
        Month = v;
      }},{get: function () {
        return Day;
      }, set: function (v) {
        Day = v;
      }},NumberOfMonths);
    Result = $mod.ComposeDateTime($impl.DoEncodeDate(Year,Month,Day),DateTime);
    return Result;
  };
  this.IncAMonth = function (Year, Month, Day, NumberOfMonths) {
    var TempMonth = 0;
    var S = 0;
    if (NumberOfMonths >= 0) {
      S = 1}
     else S = -1;
    Year.set(Year.get() + Math.floor(NumberOfMonths / 12));
    TempMonth = (Month.get() + (NumberOfMonths % 12)) - 1;
    if ((TempMonth > 11) || (TempMonth < 0)) {
      TempMonth -= S * 12;
      Year.set(Year.get() + S);
    };
    Month.set(TempMonth + 1);
    if (Day.get() > $mod.MonthDays[+$mod.IsLeapYear(Year.get())][Month.get() - 1]) Day.set($mod.MonthDays[+$mod.IsLeapYear(Year.get())][Month.get() - 1]);
  };
  this.IsLeapYear = function (Year) {
    var Result = false;
    Result = ((Year % 4) === 0) && (((Year % 100) !== 0) || ((Year % 400) === 0));
    return Result;
  };
  this.DateToStr = function (date) {
    var Result = "";
    Result = $mod.FormatDateTime("ddddd",date);
    return Result;
  };
  this.TimeToStr = function (Time) {
    var Result = "";
    Result = $mod.FormatDateTime("tt",Time);
    return Result;
  };
  this.DateTimeToStr = function (DateTime, ForceTimeIfZero) {
    var Result = "";
    Result = $mod.FormatDateTime($impl.DateTimeToStrFormat[+ForceTimeIfZero],DateTime);
    return Result;
  };
  this.StrToDate = function (S) {
    var Result = 0.0;
    Result = $mod.StrToDate$2(S,$mod.ShortDateFormat,"\x00");
    return Result;
  };
  this.StrToDate$1 = function (S, separator) {
    var Result = 0.0;
    Result = $mod.StrToDate$2(S,$mod.ShortDateFormat,separator);
    return Result;
  };
  this.StrToDate$2 = function (S, useformat, separator) {
    var Result = 0.0;
    var MSg = "";
    Result = $impl.IntStrToDate({get: function () {
        return MSg;
      }, set: function (v) {
        MSg = v;
      }},S,useformat,separator);
    if (MSg !== "") throw $mod.EConvertError.$create("Create$1",[MSg]);
    return Result;
  };
  this.StrToTime = function (S) {
    var Result = 0.0;
    Result = $mod.StrToTime$1(S,$mod.TimeSeparator);
    return Result;
  };
  this.StrToTime$1 = function (S, separator) {
    var Result = 0.0;
    var Msg = "";
    Result = $impl.IntStrToTime({get: function () {
        return Msg;
      }, set: function (v) {
        Msg = v;
      }},S,S.length,separator);
    if (Msg !== "") throw $mod.EConvertError.$create("Create$1",[Msg]);
    return Result;
  };
  this.StrToDateTime = function (S) {
    var Result = 0.0;
    var TimeStr = "";
    var DateStr = "";
    var PartsFound = 0;
    PartsFound = $impl.SplitDateTimeStr(S,{get: function () {
        return DateStr;
      }, set: function (v) {
        DateStr = v;
      }},{get: function () {
        return TimeStr;
      }, set: function (v) {
        TimeStr = v;
      }});
    var $tmp1 = PartsFound;
    if ($tmp1 === 0) {
      Result = $mod.StrToDate("")}
     else if ($tmp1 === 1) {
      if (DateStr.length > 0) {
        Result = $mod.StrToDate$2(DateStr,$mod.ShortDateFormat,$mod.DateSeparator)}
       else Result = $mod.StrToTime(TimeStr)}
     else if ($tmp1 === 2) Result = $mod.ComposeDateTime($mod.StrToDate$2(DateStr,$mod.ShortDateFormat,$mod.DateSeparator),$mod.StrToTime(TimeStr));
    return Result;
  };
  this.FormatDateTime = function (FormatStr, DateTime) {
    var Result = "";
    function StoreStr(APos, Len) {
      Result = Result + pas.System.Copy(FormatStr,APos,Len);
    };
    function StoreString(AStr) {
      Result = Result + AStr;
    };
    function StoreInt(Value, Digits) {
      var S = "";
      S = $mod.IntToStr(Value);
      while (S.length < Digits) S = "0" + S;
      StoreString(S);
    };
    var Year = 0;
    var Month = 0;
    var Day = 0;
    var DayOfWeek = 0;
    var Hour = 0;
    var Minute = 0;
    var Second = 0;
    var MilliSecond = 0;
    function StoreFormat(FormatStr, Nesting, TimeFlag) {
      var Token = "";
      var lastformattoken = "";
      var prevlasttoken = "";
      var Count = 0;
      var Clock12 = false;
      var tmp = 0;
      var isInterval = false;
      var P = 0;
      var FormatCurrent = 0;
      var FormatEnd = 0;
      if (Nesting > 1) return;
      FormatCurrent = 1;
      FormatEnd = FormatStr.length;
      Clock12 = false;
      isInterval = false;
      P = 1;
      while (P <= FormatEnd) {
        Token = FormatStr.charAt(P - 1);
        var $tmp1 = Token;
        if (($tmp1 === "'") || ($tmp1 === '"')) {
          P += 1;
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
        } else if (($tmp1 === "A") || ($tmp1 === "a")) {
          if ((($mod.CompareText(pas.System.Copy(FormatStr,P,3),"A\/P") === 0) || ($mod.CompareText(pas.System.Copy(FormatStr,P,4),"AMPM") === 0)) || ($mod.CompareText(pas.System.Copy(FormatStr,P,5),"AM\/PM") === 0)) {
            Clock12 = true;
            break;
          };
        };
        P += 1;
      };
      Token = "ÿ";
      lastformattoken = " ";
      prevlasttoken = "H";
      while (FormatCurrent <= FormatEnd) {
        Token = $mod.UpperCase(FormatStr.charAt(FormatCurrent - 1)).charAt(0);
        Count = 1;
        P = FormatCurrent + 1;
        var $tmp2 = Token;
        if (($tmp2 === "'") || ($tmp2 === '"')) {
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
          P += 1;
          Count = P - FormatCurrent;
          StoreStr(FormatCurrent + 1,Count - 2);
        } else if ($tmp2 === "A") {
          if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,4),"AMPM") === 0) {
            Count = 4;
            if (Hour < 12) {
              StoreString($mod.TimeAMString)}
             else StoreString($mod.TimePMString);
          } else if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,5),"AM\/PM") === 0) {
            Count = 5;
            if (Hour < 12) {
              StoreStr(FormatCurrent,2)}
             else StoreStr(FormatCurrent + 3,2);
          } else if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,3),"A\/P") === 0) {
            Count = 3;
            if (Hour < 12) {
              StoreStr(FormatCurrent,1)}
             else StoreStr(FormatCurrent + 2,1);
          } else throw $mod.EConvertError.$create("Create$1",["Illegal character in format string"]);
        } else if ($tmp2 === "\/") {
          StoreString($mod.DateSeparator);
        } else if ($tmp2 === ":") {
          StoreString($mod.TimeSeparator)}
         else if ((((((((((($tmp2 === " ") || ($tmp2 === "C")) || ($tmp2 === "D")) || ($tmp2 === "H")) || ($tmp2 === "M")) || ($tmp2 === "N")) || ($tmp2 === "S")) || ($tmp2 === "T")) || ($tmp2 === "Y")) || ($tmp2 === "Z")) || ($tmp2 === "F")) {
          while ((P <= FormatEnd) && ($mod.UpperCase(FormatStr.charAt(P - 1)) === Token)) P += 1;
          Count = P - FormatCurrent;
          var $tmp3 = Token;
          if ($tmp3 === " ") {
            StoreStr(FormatCurrent,Count)}
           else if ($tmp3 === "Y") {
            if (Count > 2) {
              StoreInt(Year,4)}
             else StoreInt(Year % 100,2);
          } else if ($tmp3 === "M") {
            if (isInterval && ((prevlasttoken === "H") || TimeFlag)) {
              StoreInt(Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60),0)}
             else if ((lastformattoken === "H") || TimeFlag) {
              if (Count === 1) {
                StoreInt(Minute,0)}
               else StoreInt(Minute,2);
            } else {
              var $tmp4 = Count;
              if ($tmp4 === 1) {
                StoreInt(Month,0)}
               else if ($tmp4 === 2) {
                StoreInt(Month,2)}
               else if ($tmp4 === 3) {
                StoreString($mod.ShortMonthNames[Month - 1])}
               else {
                StoreString($mod.LongMonthNames[Month - 1]);
              };
            };
          } else if ($tmp3 === "D") {
            var $tmp5 = Count;
            if ($tmp5 === 1) {
              StoreInt(Day,0)}
             else if ($tmp5 === 2) {
              StoreInt(Day,2)}
             else if ($tmp5 === 3) {
              StoreString($mod.ShortDayNames[DayOfWeek])}
             else if ($tmp5 === 4) {
              StoreString($mod.LongDayNames[DayOfWeek])}
             else if ($tmp5 === 5) {
              StoreFormat($mod.ShortDateFormat,Nesting + 1,false)}
             else {
              StoreFormat($mod.LongDateFormat,Nesting + 1,false);
            };
          } else if ($tmp3 === "H") {
            if (isInterval) {
              StoreInt(Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24),0)}
             else if (Clock12) {
              tmp = Hour % 12;
              if (tmp === 0) tmp = 12;
              if (Count === 1) {
                StoreInt(tmp,0)}
               else StoreInt(tmp,2);
            } else {
              if (Count === 1) {
                StoreInt(Hour,0)}
               else StoreInt(Hour,2);
            }}
           else if ($tmp3 === "N") {
            if (isInterval) {
              StoreInt(Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Minute,0)}
             else StoreInt(Minute,2)}
           else if ($tmp3 === "S") {
            if (isInterval) {
              StoreInt(Second + ((Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Second,0)}
             else StoreInt(Second,2)}
           else if ($tmp3 === "Z") {
            if (Count === 1) {
              StoreInt(MilliSecond,0)}
             else StoreInt(MilliSecond,3)}
           else if ($tmp3 === "T") {
            if (Count === 1) {
              StoreFormat($mod.ShortTimeFormat,Nesting + 1,true)}
             else StoreFormat($mod.LongTimeFormat,Nesting + 1,true)}
           else if ($tmp3 === "C") {
            StoreFormat($mod.ShortDateFormat,Nesting + 1,false);
            if (((Hour !== 0) || (Minute !== 0)) || (Second !== 0)) {
              StoreString(" ");
              StoreFormat($mod.LongTimeFormat,Nesting + 1,true);
            };
          } else if ($tmp3 === "F") {
            StoreFormat($mod.ShortDateFormat,Nesting + 1,false);
            StoreString(" ");
            StoreFormat($mod.LongTimeFormat,Nesting + 1,true);
          };
          prevlasttoken = lastformattoken;
          lastformattoken = Token;
        } else {
          StoreString(Token);
        };
        FormatCurrent += Count;
      };
    };
    $mod.DecodeDateFully(DateTime,{get: function () {
        return Year;
      }, set: function (v) {
        Year = v;
      }},{get: function () {
        return Month;
      }, set: function (v) {
        Month = v;
      }},{get: function () {
        return Day;
      }, set: function (v) {
        Day = v;
      }},{get: function () {
        return DayOfWeek;
      }, set: function (v) {
        DayOfWeek = v;
      }});
    $mod.DecodeTime(DateTime,{get: function () {
        return Hour;
      }, set: function (v) {
        Hour = v;
      }},{get: function () {
        return Minute;
      }, set: function (v) {
        Minute = v;
      }},{get: function () {
        return Second;
      }, set: function (v) {
        Second = v;
      }},{get: function () {
        return MilliSecond;
      }, set: function (v) {
        MilliSecond = v;
      }});
    if (FormatStr !== "") {
      StoreFormat(FormatStr,0,false)}
     else StoreFormat("C",0,false);
    return Result;
  };
  this.TryStrToDate = function (S, Value) {
    var Result = false;
    Result = $mod.TryStrToDate$2(S,Value,$mod.ShortDateFormat,"\x00");
    return Result;
  };
  this.TryStrToDate$1 = function (S, Value, separator) {
    var Result = false;
    Result = $mod.TryStrToDate$2(S,Value,$mod.ShortDateFormat,separator);
    return Result;
  };
  this.TryStrToDate$2 = function (S, Value, useformat, separator) {
    var Result = false;
    var Msg = "";
    Result = S.length !== 0;
    if (Result) {
      Value.set($impl.IntStrToDate({get: function () {
          return Msg;
        }, set: function (v) {
          Msg = v;
        }},S,useformat,separator));
      Result = Msg === "";
    };
    return Result;
  };
  this.TryStrToTime = function (S, Value) {
    var Result = false;
    Result = $mod.TryStrToTime$1(S,Value,"\x00");
    return Result;
  };
  this.TryStrToTime$1 = function (S, Value, separator) {
    var Result = false;
    var Msg = "";
    Result = S.length !== 0;
    if (Result) {
      Value.set($impl.IntStrToTime({get: function () {
          return Msg;
        }, set: function (v) {
          Msg = v;
        }},S,S.length,separator));
      Result = Msg === "";
    };
    return Result;
  };
  this.TryStrToDateTime = function (S, Value) {
    var Result = false;
    var I = 0;
    var dtdate = 0.0;
    var dttime = 0.0;
    Result = false;
    I = pas.System.Pos($mod.TimeSeparator,S);
    if (I > 0) {
      while ((I > 0) && (S.charAt(I - 1) !== " ")) I -= 1;
      if (I > 0) {
        if (!$mod.TryStrToDate(pas.System.Copy(S,1,I - 1),{get: function () {
            return dtdate;
          }, set: function (v) {
            dtdate = v;
          }})) return Result;
        if (!$mod.TryStrToTime(pas.System.Copy(S,I + 1,S.length - I),{get: function () {
            return dttime;
          }, set: function (v) {
            dttime = v;
          }})) return Result;
        Value.set($mod.ComposeDateTime(dtdate,dttime));
        Result = true;
      } else Result = $mod.TryStrToTime(S,Value);
    } else Result = $mod.TryStrToDate(S,Value);
    return Result;
  };
  this.StrToDateDef = function (S, Defvalue) {
    var Result = 0.0;
    Result = $mod.StrToDateDef$1(S,Defvalue,"\x00");
    return Result;
  };
  this.StrToDateDef$1 = function (S, Defvalue, separator) {
    var Result = 0.0;
    if (!$mod.TryStrToDate$1(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},separator)) Result = Defvalue;
    return Result;
  };
  this.StrToTimeDef = function (S, Defvalue) {
    var Result = 0.0;
    Result = $mod.StrToTimeDef$1(S,Defvalue,"\x00");
    return Result;
  };
  this.StrToTimeDef$1 = function (S, Defvalue, separator) {
    var Result = 0.0;
    if (!$mod.TryStrToTime$1(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},separator)) Result = Defvalue;
    return Result;
  };
  this.StrToDateTimeDef = function (S, Defvalue) {
    var Result = 0.0;
    if (!$mod.TryStrToDateTime(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = Defvalue;
    return Result;
  };
  this.CurrentYear = function () {
    var Result = 0;
    Result = (new Date()).getFullYear();
    return Result;
  };
  this.ReplaceTime = function (dati, NewTime) {
    dati.set($mod.ComposeDateTime(dati.get(),NewTime));
  };
  this.ReplaceDate = function (DateTime, NewDate) {
    var tmp = 0.0;
    tmp = NewDate;
    $mod.ReplaceTime({get: function () {
        return tmp;
      }, set: function (v) {
        tmp = v;
      }},DateTime.get());
    DateTime.set(tmp);
  };
  this.FloatToDateTime = function (Value) {
    var Result = 0.0;
    if ((Value < $mod.MinDateTime) || (Value > $mod.MaxDateTime)) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidDateTime,[$mod.FloatToStr(Value)]]);
    Result = Value;
    return Result;
  };
  $mod.$init = function () {
    $mod.FormatSettings = $mod.TFormatSettings.$create("Create");
    rtl.EInvalidCast = $mod.EInvalidCast;
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.SAbortError = "Operation aborted";
  $impl.CheckBoolStrs = function () {
    if (rtl.length($mod.TrueBoolStrs) === 0) {
      $mod.TrueBoolStrs = rtl.arraySetLength($mod.TrueBoolStrs,"",1);
      $mod.TrueBoolStrs[0] = "True";
    };
    if (rtl.length($mod.FalseBoolStrs) === 0) {
      $mod.FalseBoolStrs = rtl.arraySetLength($mod.FalseBoolStrs,"",1);
      $mod.FalseBoolStrs[0] = "False";
    };
  };
  $impl.feInvalidFormat = 1;
  $impl.feMissingArgument = 2;
  $impl.feInvalidArgIndex = 3;
  $impl.DoFormatError = function (ErrCode, fmt) {
    var $tmp1 = ErrCode;
    if ($tmp1 === 1) {
      throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidFormat,[fmt]])}
     else if ($tmp1 === 2) {
      throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SArgumentMissing,[fmt]])}
     else if ($tmp1 === 3) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidArgIndex,[fmt]]);
  };
  $impl.maxdigits = 15;
  $impl.ReplaceDecimalSep = function (S, DS) {
    var Result = "";
    var P = 0;
    P = pas.System.Pos(".",S);
    if (P > 0) {
      Result = (pas.System.Copy(S,1,P - 1) + DS) + pas.System.Copy(S,P + 1,S.length - P)}
     else Result = S;
    return Result;
  };
  $impl.FormatGeneralFloat = function (Value, Precision, DS) {
    var Result = "";
    var P = 0;
    var PE = 0;
    var Q = 0;
    var Exponent = 0;
    if ((Precision === -1) || (Precision > 15)) Precision = 15;
    Result = rtl.floatToStr(Value,Precision + 7);
    Result = $mod.TrimLeft(Result);
    P = pas.System.Pos(".",Result);
    if (P === 0) return Result;
    PE = pas.System.Pos("E",Result);
    if (PE === 0) {
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    Q = PE + 2;
    Exponent = 0;
    while (Q <= Result.length) {
      Exponent = ((Exponent * 10) + Result.charCodeAt(Q - 1)) - "0".charCodeAt();
      Q += 1;
    };
    if (Result.charAt((PE + 1) - 1) === "-") Exponent = -Exponent;
    if (((P + Exponent) < PE) && (Exponent > -6)) {
      Result = rtl.strSetLength(Result,PE - 1);
      if (Exponent >= 0) {
        for (var $l1 = 0, $end2 = Exponent - 1; $l1 <= $end2; $l1++) {
          Q = $l1;
          Result = rtl.setCharAt(Result,P - 1,Result.charAt((P + 1) - 1));
          P += 1;
        };
        Result = rtl.setCharAt(Result,P - 1,".");
        P = 1;
        if (Result.charAt(P - 1) === "-") P += 1;
        while (((Result.charAt(P - 1) === "0") && (P < Result.length)) && (pas.System.Copy(Result,P + 1,DS.length) !== DS)) pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P,1);
      } else {
        pas.System.Insert(pas.System.Copy("00000",1,-Exponent),{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P - 1);
        Result = rtl.setCharAt(Result,(P - Exponent) - 1,Result.charAt(((P - Exponent) - 1) - 1));
        Result = rtl.setCharAt(Result,P - 1,".");
        if (Exponent !== -1) Result = rtl.setCharAt(Result,((P - Exponent) - 1) - 1,"0");
      };
      Q = Result.length;
      while ((Q > 0) && (Result.charAt(Q - 1) === "0")) Q -= 1;
      if (Result.charAt(Q - 1) === DS) Q -= 1;
      if ((Q === 0) || ((Q === 1) && (Result.charAt(0) === "-"))) {
        Result = "0"}
       else Result = rtl.strSetLength(Result,Q);
    } else {
      while (Result.charAt((PE - 1) - 1) === "0") {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE - 1,1);
        PE -= 1;
      };
      if (Result.charAt((PE - 1) - 1) === DS) {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE - 1,1);
        PE -= 1;
      };
      if (Result.charAt((PE + 1) - 1) === "+") {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE + 1,1)}
       else PE += 1;
      while (Result.charAt((PE + 1) - 1) === "0") pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},PE + 1,1);
    };
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatExponentFloat = function (Value, Precision, Digits, DS) {
    var Result = "";
    var P = 0;
    DS = $mod.DecimalSeparator;
    if ((Precision === -1) || (Precision > 15)) Precision = 15;
    Result = rtl.floatToStr(Value,Precision + 7);
    while (Result.charAt(0) === " ") pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos("E",Result);
    if (P === 0) {
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    P += 2;
    if (Digits > 4) Digits = 4;
    Digits = ((Result.length - P) - Digits) + 1;
    if (Digits < 0) {
      pas.System.Insert(pas.System.Copy("0000",1,-Digits),{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P)}
     else while ((Digits > 0) && (Result.charAt(P - 1) === "0")) {
      pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P,1);
      if (P > Result.length) {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P - 2,2);
        break;
      };
      Digits -= 1;
    };
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatFixedFloat = function (Value, Digits, DS) {
    var Result = "";
    if (Digits === -1) {
      Digits = 2}
     else if (Digits > 18) Digits = 18;
    Result = rtl.floatToStr(Value,0,Digits);
    if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatNumberFloat = function (Value, Digits, DS, TS) {
    var Result = "";
    var P = 0;
    if (Digits === -1) {
      Digits = 2}
     else if (Digits > 15) Digits = 15;
    Result = rtl.floatToStr(Value,0,Digits);
    if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos(".",Result);
    Result = $impl.ReplaceDecimalSep(Result,DS);
    P -= 3;
    if ((TS !== "") && (TS !== "\x00")) while (P > 1) {
      if (Result.charAt((P - 1) - 1) !== "-") pas.System.Insert(TS,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P);
      P -= 3;
    };
    return Result;
  };
  $impl.RESpecials = "([\\[\\]\\(\\)\\\\\\.\\*])";
  $impl.DoEncodeDate = function (Year, Month, Day) {
    var Result = 0;
    var D = 0.0;
    if ($mod.TryEncodeDate(Year,Month,Day,{get: function () {
        return D;
      }, set: function (v) {
        D = v;
      }})) {
      Result = pas.System.Trunc(D)}
     else Result = 0;
    return Result;
  };
  $impl.DoEncodeTime = function (Hour, Minute, Second, MilliSecond) {
    var Result = 0.0;
    if (!$mod.TryEncodeTime(Hour,Minute,Second,MilliSecond,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = 0;
    return Result;
  };
  $impl.DateTimeToStrFormat = ["c","f"];
  var WhiteSpace = " \b\t\n\f\r";
  var Digits = "0123456789";
  $impl.IntStrToDate = function (ErrorMsg, S, useformat, separator) {
    var Result = 0.0;
    function FixErrorMsg(errmarg) {
      ErrorMsg.set($mod.Format(pas.RTLConsts.SInvalidDateFormat,[errmarg]));
    };
    var df = "";
    var d = 0;
    var m = 0;
    var y = 0;
    var ly = 0;
    var ld = 0;
    var lm = 0;
    var n = 0;
    var i = 0;
    var len = 0;
    var c = 0;
    var dp = 0;
    var mp = 0;
    var yp = 0;
    var which = 0;
    var s1 = "";
    var values = [];
    var YearMoreThenTwoDigits = false;
    values = rtl.arraySetLength(values,0,4);
    Result = 0;
    len = S.length;
    ErrorMsg.set("");
    while ((len > 0) && (pas.System.Pos(S.charAt(len - 1),WhiteSpace) > 0)) len -= 1;
    if (len === 0) {
      FixErrorMsg(S);
      return Result;
    };
    YearMoreThenTwoDigits = false;
    if (separator === "\x00") if ($mod.DateSeparator !== "\x00") {
      separator = $mod.DateSeparator}
     else separator = "-";
    df = $mod.UpperCase(useformat);
    yp = 0;
    mp = 0;
    dp = 0;
    which = 0;
    i = 0;
    while ((i < df.length) && (which < 3)) {
      i += 1;
      var $tmp1 = df.charAt(i - 1);
      if ($tmp1 === "Y") {
        if (yp === 0) {
          which += 1;
          yp = which;
        }}
       else if ($tmp1 === "M") {
        if (mp === 0) {
          which += 1;
          mp = which;
        }}
       else if ($tmp1 === "D") if (dp === 0) {
        which += 1;
        dp = which;
      };
    };
    for (i = 1; i <= 3; i++) values[i] = 0;
    s1 = "";
    n = 0;
    for (var $l2 = 1, $end3 = len; $l2 <= $end3; $l2++) {
      i = $l2;
      if (pas.System.Pos(S.charAt(i - 1),Digits) > 0) s1 = s1 + S.charAt(i - 1);
      if ((separator !== " ") && (S.charAt(i - 1) === " ")) continue;
      if ((S.charAt(i - 1) === separator) || ((i === len) && (pas.System.Pos(S.charAt(i - 1),Digits) > 0))) {
        n += 1;
        if (n > 3) {
          FixErrorMsg(S);
          return Result;
        };
        if ((n === yp) && (s1.length > 2)) YearMoreThenTwoDigits = true;
        pas.System.val$5(s1,{a: n, p: values, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }},{get: function () {
            return c;
          }, set: function (v) {
            c = v;
          }});
        if (c !== 0) {
          FixErrorMsg(S);
          return Result;
        };
        s1 = "";
      } else if (pas.System.Pos(S.charAt(i - 1),Digits) === 0) {
        FixErrorMsg(S);
        return Result;
      };
    };
    if ((which < 3) && (n > which)) {
      FixErrorMsg(S);
      return Result;
    };
    $mod.DecodeDate($mod.date(),{get: function () {
        return ly;
      }, set: function (v) {
        ly = v;
      }},{get: function () {
        return lm;
      }, set: function (v) {
        lm = v;
      }},{get: function () {
        return ld;
      }, set: function (v) {
        ld = v;
      }});
    if (n === 3) {
      y = values[yp];
      m = values[mp];
      d = values[dp];
    } else {
      y = ly;
      if (n < 2) {
        d = values[1];
        m = lm;
      } else if (dp < mp) {
        d = values[1];
        m = values[2];
      } else {
        d = values[2];
        m = values[1];
      };
    };
    if (((y >= 0) && (y < 100)) && !YearMoreThenTwoDigits) {
      ly = ly - $mod.TwoDigitYearCenturyWindow;
      y += Math.floor(ly / 100) * 100;
      if (($mod.TwoDigitYearCenturyWindow > 0) && (y < ly)) y += 100;
    };
    if (!$mod.TryEncodeDate(y,m,d,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) ErrorMsg.set(pas.RTLConsts.SErrInvalidDate);
    return Result;
  };
  var AMPM_None = 0;
  var AMPM_AM = 1;
  var AMPM_PM = 2;
  var tiHour = 0;
  var tiMin = 1;
  var tiSec = 2;
  var tiMSec = 3;
  var Digits = "0123456789";
  $impl.IntStrToTime = function (ErrorMsg, S, Len, separator) {
    var Result = 0.0;
    var AmPm = 0;
    var TimeValues = [];
    function SplitElements(TimeValues, AmPm) {
      var Result = false;
      var Cur = 0;
      var Offset = 0;
      var ElemLen = 0;
      var Err = 0;
      var TimeIndex = 0;
      var FirstSignificantDigit = 0;
      var Value = 0;
      var DigitPending = false;
      var MSecPending = false;
      var AmPmStr = "";
      var CurChar = "";
      var I = 0;
      var allowedchars = "";
      Result = false;
      AmPm.set(0);
      MSecPending = false;
      TimeIndex = 0;
      for (I = 0; I <= 3; I++) TimeValues.get()[I] = 0;
      Cur = 1;
      while ((Cur < Len) && (S.charAt(Cur - 1) === " ")) Cur += 1;
      Offset = Cur;
      if (((Cur > (Len - 1)) || (S.charAt(Cur - 1) === separator)) || (S.charAt(Cur - 1) === $mod.DecimalSeparator)) {
        return Result;
      };
      DigitPending = pas.System.Pos(S.charAt(Cur - 1),Digits) > 0;
      while (Cur <= Len) {
        CurChar = S.charAt(Cur - 1);
        if (pas.System.Pos(CurChar,Digits) > 0) {
          if (!DigitPending || (TimeIndex > 3)) {
            return Result;
          };
          Offset = Cur;
          if (CurChar !== "0") {
            FirstSignificantDigit = Offset}
           else FirstSignificantDigit = -1;
          while ((Cur < Len) && (pas.System.Pos(S.charAt((Cur + 1) - 1),Digits) > 0)) {
            if ((FirstSignificantDigit === -1) && (S.charAt(Cur - 1) !== "0")) FirstSignificantDigit = Cur;
            Cur += 1;
          };
          if (FirstSignificantDigit === -1) FirstSignificantDigit = Cur;
          ElemLen = (1 + Cur) - FirstSignificantDigit;
          if ((ElemLen <= 2) || ((ElemLen <= 3) && (TimeIndex === 3))) {
            pas.System.val$5(pas.System.Copy(S,FirstSignificantDigit,ElemLen),{get: function () {
                return Value;
              }, set: function (v) {
                Value = v;
              }},{get: function () {
                return Err;
              }, set: function (v) {
                Err = v;
              }});
            TimeValues.get()[TimeIndex] = Value;
            TimeIndex += 1;
            DigitPending = false;
          } else {
            return Result;
          };
        } else if (CurChar === " ") {}
        else if (CurChar === separator) {
          if (DigitPending || (TimeIndex > 2)) {
            return Result;
          };
          DigitPending = true;
          MSecPending = false;
        } else if (CurChar === $mod.DecimalSeparator) {
          if ((DigitPending || MSecPending) || (TimeIndex !== 3)) {
            return Result;
          };
          DigitPending = true;
          MSecPending = true;
        } else {
          if ((AmPm.get() !== 0) || DigitPending) {
            return Result;
          };
          Offset = Cur;
          allowedchars = $mod.DecimalSeparator + " ";
          if (separator !== "\x00") allowedchars = allowedchars + separator;
          while (((Cur < (Len - 1)) && (pas.System.Pos(S.charAt((Cur + 1) - 1),allowedchars) === 0)) && (pas.System.Pos(S.charAt((Cur + 1) - 1),Digits) === 0)) Cur += 1;
          ElemLen = (1 + Cur) - Offset;
          AmPmStr = pas.System.Copy(S,1 + Offset,ElemLen);
          if ($mod.CompareText(AmPmStr,$mod.TimeAMString) === 0) {
            AmPm.set(1)}
           else if ($mod.CompareText(AmPmStr,$mod.TimePMString) === 0) {
            AmPm.set(2)}
           else if ($mod.CompareText(AmPmStr,"AM") === 0) {
            AmPm.set(1)}
           else if ($mod.CompareText(AmPmStr,"PM") === 0) {
            AmPm.set(2)}
           else {
            return Result;
          };
          if (TimeIndex === 0) {
            DigitPending = true;
          } else {
            TimeIndex = 3 + 1;
            DigitPending = false;
          };
        };
        Cur += 1;
      };
      if (((TimeIndex === 0) || ((AmPm.get() !== 0) && ((TimeValues.get()[0] > 12) || (TimeValues.get()[0] === 0)))) || DigitPending) return Result;
      Result = true;
      return Result;
    };
    TimeValues = rtl.arraySetLength(TimeValues,0,4);
    if (separator === "\x00") if ($mod.TimeSeparator !== "\x00") {
      separator = $mod.TimeSeparator}
     else separator = ":";
    AmPm = 0;
    if (!SplitElements({get: function () {
        return TimeValues;
      }, set: function (v) {
        TimeValues = v;
      }},{get: function () {
        return AmPm;
      }, set: function (v) {
        AmPm = v;
      }})) {
      ErrorMsg.set($mod.Format(pas.RTLConsts.SErrInvalidTimeFormat,[S]));
      return Result;
    };
    if ((AmPm === 2) && (TimeValues[0] !== 12)) {
      TimeValues[0] += 12}
     else if ((AmPm === 1) && (TimeValues[0] === 12)) TimeValues[0] = 0;
    if (!$mod.TryEncodeTime(TimeValues[0],TimeValues[1],TimeValues[2],TimeValues[3],{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) ErrorMsg.set($mod.Format(pas.RTLConsts.SErrInvalidTimeFormat,[S]));
    return Result;
  };
  var WhiteSpace = "\t\n\r ";
  $impl.SplitDateTimeStr = function (DateTimeStr, DateStr, TimeStr) {
    var Result = 0;
    var p = 0;
    var DummyDT = 0.0;
    Result = 0;
    DateStr.set("");
    TimeStr.set("");
    DateTimeStr = $mod.Trim(DateTimeStr);
    if (DateTimeStr.length === 0) return Result;
    if ((($mod.DateSeparator === " ") && ($mod.TimeSeparator === " ")) && (pas.System.Pos(" ",DateTimeStr) > 0)) {
      DateStr.set(DateTimeStr);
      return 1;
    };
    p = 1;
    if ($mod.DateSeparator !== " ") {
      while ((p < DateTimeStr.length) && !(pas.System.Pos(DateTimeStr.charAt((p + 1) - 1),WhiteSpace) > 0)) p += 1;
    } else {
      p = pas.System.Pos($mod.TimeSeparator,DateTimeStr);
      if (p !== 0) do {
        p -= 1;
      } while (!((p === 0) || (pas.System.Pos(DateTimeStr.charAt(p - 1),WhiteSpace) > 0)));
    };
    if (p === 0) p = DateTimeStr.length;
    DateStr.set(pas.System.Copy(DateTimeStr,1,p));
    TimeStr.set($mod.Trim(pas.System.Copy(DateTimeStr,p + 1,100)));
    if (TimeStr.get().length !== 0) {
      Result = 2}
     else {
      Result = 1;
      if ((($mod.DateSeparator !== $mod.TimeSeparator) && (pas.System.Pos($mod.TimeSeparator,DateStr.get()) > 0)) || (($mod.DateSeparator === $mod.TimeSeparator) && !$mod.TryStrToDate(DateStr.get(),{get: function () {
          return DummyDT;
        }, set: function (v) {
          DummyDT = v;
        }}))) {
        TimeStr.set(DateStr.get());
        DateStr.set("");
      };
    };
    return Result;
  };
});
rtl.module("Classes",["System","RTLConsts","Types","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TFPObservedOperation = {"0": "ooChange", ooChange: 0, "1": "ooFree", ooFree: 1, "2": "ooAddItem", ooAddItem: 2, "3": "ooDeleteItem", ooDeleteItem: 3, "4": "ooCustom", ooCustom: 4};
  rtl.createClass($mod,"EListError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass($mod,"EStringListError",$mod.EListError,function () {
  });
  rtl.createClass($mod,"EComponentError",pas.SysUtils.Exception,function () {
  });
  this.TListAssignOp = {"0": "laCopy", laCopy: 0, "1": "laAnd", laAnd: 1, "2": "laOr", laOr: 2, "3": "laXor", laXor: 3, "4": "laSrcUnique", laSrcUnique: 4, "5": "laDestUnique", laDestUnique: 5};
  this.TAlignment = {"0": "taLeftJustify", taLeftJustify: 0, "1": "taRightJustify", taRightJustify: 1, "2": "taCenter", taCenter: 2};
  rtl.createClass($mod,"TFPListEnumerator",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = null;
      this.FPosition = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (AList) {
      pas.System.TObject.Create.call(this);
      this.FList = AList;
      this.FPosition = -1;
    };
    this.GetCurrent = function () {
      var Result = undefined;
      Result = this.FList.Get(this.FPosition);
      return Result;
    };
    this.MoveNext = function () {
      var Result = false;
      this.FPosition += 1;
      Result = this.FPosition < this.FList.FCount;
      return Result;
    };
  });
  rtl.createClass($mod,"TFPList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = [];
      this.FCount = 0;
      this.FCapacity = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.CopyMove = function (aList) {
      var r = 0;
      this.Clear();
      for (var $l1 = 0, $end2 = aList.FCount - 1; $l1 <= $end2; $l1++) {
        r = $l1;
        this.Add(aList.Get(r));
      };
    };
    this.MergeMove = function (aList) {
      var r = 0;
      for (var $l1 = 0, $end2 = aList.FCount - 1; $l1 <= $end2; $l1++) {
        r = $l1;
        if (this.IndexOf(aList.Get(r)) < 0) this.Add(aList.Get(r));
      };
    };
    this.DoCopy = function (ListA, ListB) {
      if (ListB != null) {
        this.CopyMove(ListB)}
       else this.CopyMove(ListA);
    };
    this.DoSrcUnique = function (ListA, ListB) {
      var r = 0;
      if (ListB != null) {
        this.Clear();
        for (var $l1 = 0, $end2 = ListA.FCount - 1; $l1 <= $end2; $l1++) {
          r = $l1;
          if (ListB.IndexOf(ListA.Get(r)) < 0) this.Add(ListA.Get(r));
        };
      } else {
        for (var $l3 = this.FCount - 1; $l3 >= 0; $l3--) {
          r = $l3;
          if (ListA.IndexOf(this.Get(r)) >= 0) this.Delete(r);
        };
      };
    };
    this.DoAnd = function (ListA, ListB) {
      var r = 0;
      if (ListB != null) {
        this.Clear();
        for (var $l1 = 0, $end2 = ListA.FCount - 1; $l1 <= $end2; $l1++) {
          r = $l1;
          if (ListB.IndexOf(ListA.Get(r)) >= 0) this.Add(ListA.Get(r));
        };
      } else {
        for (var $l3 = this.FCount - 1; $l3 >= 0; $l3--) {
          r = $l3;
          if (ListA.IndexOf(this.Get(r)) < 0) this.Delete(r);
        };
      };
    };
    this.DoDestUnique = function (ListA, ListB) {
      var Self = this;
      function MoveElements(Src, Dest) {
        var r = 0;
        Self.Clear();
        for (var $l1 = 0, $end2 = Src.FCount - 1; $l1 <= $end2; $l1++) {
          r = $l1;
          if (Dest.IndexOf(Src.Get(r)) < 0) Self.Add(Src.Get(r));
        };
      };
      var Dest = null;
      if (ListB != null) {
        MoveElements(ListB,ListA)}
       else Dest = $mod.TFPList.$create("Create");
      try {
        Dest.CopyMove(Self);
        MoveElements(ListA,Dest);
      } finally {
        Dest.$destroy("Destroy");
      };
    };
    this.DoOr = function (ListA, ListB) {
      if (ListB != null) {
        this.CopyMove(ListA);
        this.MergeMove(ListB);
      } else this.MergeMove(ListA);
    };
    this.DoXOr = function (ListA, ListB) {
      var r = 0;
      var l = null;
      if (ListB != null) {
        this.Clear();
        for (var $l1 = 0, $end2 = ListA.FCount - 1; $l1 <= $end2; $l1++) {
          r = $l1;
          if (ListB.IndexOf(ListA.Get(r)) < 0) this.Add(ListA.Get(r));
        };
        for (var $l3 = 0, $end4 = ListB.FCount - 1; $l3 <= $end4; $l3++) {
          r = $l3;
          if (ListA.IndexOf(ListB.Get(r)) < 0) this.Add(ListB.Get(r));
        };
      } else {
        l = $mod.TFPList.$create("Create");
        try {
          l.CopyMove(this);
          for (var $l5 = this.FCount - 1; $l5 >= 0; $l5--) {
            r = $l5;
            if (ListA.IndexOf(this.Get(r)) >= 0) this.Delete(r);
          };
          for (var $l6 = 0, $end7 = ListA.FCount - 1; $l6 <= $end7; $l6++) {
            r = $l6;
            if (l.IndexOf(ListA.Get(r)) < 0) this.Add(ListA.Get(r));
          };
        } finally {
          l.$destroy("Destroy");
        };
      };
    };
    this.Get = function (Index) {
      var Result = undefined;
      if ((Index < 0) || (Index >= this.FCount)) this.RaiseIndexError(Index);
      Result = this.FList[Index];
      return Result;
    };
    this.Put = function (Index, Item) {
      if ((Index < 0) || (Index >= this.FCount)) this.RaiseIndexError(Index);
      this.FList[Index] = Item;
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < this.FCount) this.$class.error(pas.RTLConsts.SListCapacityError,"" + NewCapacity);
      if (NewCapacity === this.FCapacity) return;
      this.FList = rtl.arraySetLength(this.FList,undefined,NewCapacity);
      this.FCapacity = NewCapacity;
    };
    this.SetCount = function (NewCount) {
      if (NewCount < 0) this.$class.error(pas.RTLConsts.SListCountError,"" + NewCount);
      if (NewCount > this.FCount) {
        if (NewCount > this.FCapacity) this.SetCapacity(NewCount);
      };
      this.FCount = NewCount;
    };
    this.RaiseIndexError = function (Index) {
      this.$class.error(pas.RTLConsts.SListIndexError,"" + Index);
    };
    this.Destroy = function () {
      this.Clear();
      pas.System.TObject.Destroy.call(this);
    };
    this.AddList = function (AList) {
      var I = 0;
      if (this.FCapacity < (this.FCount + AList.FCount)) this.SetCapacity(this.FCount + AList.FCount);
      for (var $l1 = 0, $end2 = AList.FCount - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        this.Add(AList.Get(I));
      };
    };
    this.Add = function (Item) {
      var Result = 0;
      if (this.FCount === this.FCapacity) this.Expand();
      this.FList[this.FCount] = Item;
      Result = this.FCount;
      this.FCount += 1;
      return Result;
    };
    this.Clear = function () {
      if (rtl.length(this.FList) > 0) {
        this.SetCount(0);
        this.SetCapacity(0);
      };
    };
    this.Delete = function (Index) {
      if ((Index < 0) || (Index >= this.FCount)) this.$class.error(pas.RTLConsts.SListIndexError,"" + Index);
      this.FCount = this.FCount - 1;
      this.FList.splice(Index,1);
      this.FCapacity -= 1;
    };
    this.error = function (Msg, Data) {
      throw $mod.EListError.$create("CreateFmt",[Msg,[Data]]);
    };
    this.Exchange = function (Index1, Index2) {
      var Temp = undefined;
      if ((Index1 >= this.FCount) || (Index1 < 0)) this.$class.error(pas.RTLConsts.SListIndexError,"" + Index1);
      if ((Index2 >= this.FCount) || (Index2 < 0)) this.$class.error(pas.RTLConsts.SListIndexError,"" + Index2);
      Temp = this.FList[Index1];
      this.FList[Index1] = this.FList[Index2];
      this.FList[Index2] = Temp;
    };
    this.Expand = function () {
      var Result = null;
      var IncSize = 0;
      if (this.FCount < this.FCapacity) return this;
      IncSize = 4;
      if (this.FCapacity > 3) IncSize = IncSize + 4;
      if (this.FCapacity > 8) IncSize = IncSize + 8;
      if (this.FCapacity > 127) IncSize += this.FCapacity >>> 2;
      this.SetCapacity(this.FCapacity + IncSize);
      Result = this;
      return Result;
    };
    this.Extract = function (Item) {
      var Result = undefined;
      var i = 0;
      i = this.IndexOf(Item);
      if (i >= 0) {
        Result = Item;
        this.Delete(i);
      } else Result = null;
      return Result;
    };
    this.First = function () {
      var Result = undefined;
      if (this.FCount === 0) {
        Result = null}
       else Result = this.Get(0);
      return Result;
    };
    this.GetEnumerator = function () {
      var Result = null;
      Result = $mod.TFPListEnumerator.$create("Create$1",[this]);
      return Result;
    };
    this.IndexOf = function (Item) {
      var Result = 0;
      var C = 0;
      Result = 0;
      C = this.FCount;
      while ((Result < C) && (this.FList[Result] != Item)) Result += 1;
      if (Result >= C) Result = -1;
      return Result;
    };
    this.IndexOfItem = function (Item, Direction) {
      var Result = 0;
      if (Direction === pas.Types.TDirection.FromBeginning) {
        Result = this.IndexOf(Item)}
       else {
        Result = this.FCount - 1;
        while ((Result >= 0) && (this.FList[Result] != Item)) Result = Result - 1;
      };
      return Result;
    };
    this.Insert = function (Index, Item) {
      if ((Index < 0) || (Index > this.FCount)) this.$class.error(pas.RTLConsts.SListIndexError,"" + Index);
      this.FList.splice(Index,0,Item);
      this.FCapacity += 1;
      this.FCount += 1;
    };
    this.Last = function () {
      var Result = undefined;
      if (this.FCount === 0) {
        Result = null}
       else Result = this.Get(this.FCount - 1);
      return Result;
    };
    this.Move = function (CurIndex, NewIndex) {
      var Temp = undefined;
      if ((CurIndex < 0) || (CurIndex > (this.FCount - 1))) this.$class.error(pas.RTLConsts.SListIndexError,"" + CurIndex);
      if ((NewIndex < 0) || (NewIndex > (this.FCount - 1))) this.$class.error(pas.RTLConsts.SListIndexError,"" + NewIndex);
      if (CurIndex === NewIndex) return;
      Temp = this.FList[CurIndex];
      this.FList.splice(CurIndex,1);
      this.FList.splice(NewIndex,0,Temp);
    };
    this.Assign = function (ListA, AOperator, ListB) {
      var $tmp1 = AOperator;
      if ($tmp1 === $mod.TListAssignOp.laCopy) {
        this.DoCopy(ListA,ListB)}
       else if ($tmp1 === $mod.TListAssignOp.laSrcUnique) {
        this.DoSrcUnique(ListA,ListB)}
       else if ($tmp1 === $mod.TListAssignOp.laAnd) {
        this.DoAnd(ListA,ListB)}
       else if ($tmp1 === $mod.TListAssignOp.laDestUnique) {
        this.DoDestUnique(ListA,ListB)}
       else if ($tmp1 === $mod.TListAssignOp.laOr) {
        this.DoOr(ListA,ListB)}
       else if ($tmp1 === $mod.TListAssignOp.laXor) this.DoXOr(ListA,ListB);
    };
    this.Remove = function (Item) {
      var Result = 0;
      Result = this.IndexOf(Item);
      if (Result !== -1) this.Delete(Result);
      return Result;
    };
    this.Pack = function () {
      var Dst = 0;
      var i = 0;
      var V = undefined;
      Dst = 0;
      for (var $l1 = 0, $end2 = this.FCount - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        V = this.FList[i];
        if (!pas.System.Assigned(V)) continue;
        this.FList[Dst] = V;
        Dst += 1;
      };
    };
    this.Sort = function (Compare) {
      if (!(rtl.length(this.FList) > 0) || (this.FCount < 2)) return;
      $impl.QuickSort(this.FList,0,this.FCount - 1,Compare);
    };
    this.ForEachCall = function (proc2call, arg) {
      var i = 0;
      var v = undefined;
      for (var $l1 = 0, $end2 = this.FCount - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        v = this.FList[i];
        if (pas.System.Assigned(v)) proc2call(v,arg);
      };
    };
    this.ForEachCall$1 = function (proc2call, arg) {
      var i = 0;
      var v = undefined;
      for (var $l1 = 0, $end2 = this.FCount - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        v = this.FList[i];
        if (pas.System.Assigned(v)) proc2call(v,arg);
      };
    };
  });
  this.TListNotification = {"0": "lnAdded", lnAdded: 0, "1": "lnExtracted", lnExtracted: 1, "2": "lnDeleted", lnDeleted: 2};
  rtl.createClass($mod,"TListEnumerator",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = null;
      this.FPosition = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (AList) {
      pas.System.TObject.Create.call(this);
      this.FList = AList;
      this.FPosition = -1;
    };
    this.GetCurrent = function () {
      var Result = undefined;
      Result = this.FList.Get(this.FPosition);
      return Result;
    };
    this.MoveNext = function () {
      var Result = false;
      this.FPosition += 1;
      Result = this.FPosition < this.FList.GetCount();
      return Result;
    };
  });
  rtl.createClass($mod,"TList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = null;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.CopyMove = function (aList) {
      var r = 0;
      this.Clear();
      for (var $l1 = 0, $end2 = aList.GetCount() - 1; $l1 <= $end2; $l1++) {
        r = $l1;
        this.Add(aList.Get(r));
      };
    };
    this.MergeMove = function (aList) {
      var r = 0;
      for (var $l1 = 0, $end2 = aList.GetCount() - 1; $l1 <= $end2; $l1++) {
        r = $l1;
        if (this.IndexOf(aList.Get(r)) < 0) this.Add(aList.Get(r));
      };
    };
    this.DoCopy = function (ListA, ListB) {
      if (ListB != null) {
        this.CopyMove(ListB)}
       else this.CopyMove(ListA);
    };
    this.DoSrcUnique = function (ListA, ListB) {
      var r = 0;
      if (ListB != null) {
        this.Clear();
        for (var $l1 = 0, $end2 = ListA.GetCount() - 1; $l1 <= $end2; $l1++) {
          r = $l1;
          if (ListB.IndexOf(ListA.Get(r)) < 0) this.Add(ListA.Get(r));
        };
      } else {
        for (var $l3 = this.GetCount() - 1; $l3 >= 0; $l3--) {
          r = $l3;
          if (ListA.IndexOf(this.Get(r)) >= 0) this.Delete(r);
        };
      };
    };
    this.DoAnd = function (ListA, ListB) {
      var r = 0;
      if (ListB != null) {
        this.Clear();
        for (var $l1 = 0, $end2 = ListA.GetCount() - 1; $l1 <= $end2; $l1++) {
          r = $l1;
          if (ListB.IndexOf(ListA.Get(r)) >= 0) this.Add(ListA.Get(r));
        };
      } else {
        for (var $l3 = this.GetCount() - 1; $l3 >= 0; $l3--) {
          r = $l3;
          if (ListA.IndexOf(this.Get(r)) < 0) this.Delete(r);
        };
      };
    };
    this.DoDestUnique = function (ListA, ListB) {
      var Self = this;
      function MoveElements(Src, Dest) {
        var r = 0;
        Self.Clear();
        for (var $l1 = 0, $end2 = Src.GetCount() - 1; $l1 <= $end2; $l1++) {
          r = $l1;
          if (Dest.IndexOf(Src.Get(r)) < 0) Self.Add(Src.Get(r));
        };
      };
      var Dest = null;
      if (ListB != null) {
        MoveElements(ListB,ListA)}
       else try {
        Dest = $mod.TList.$create("Create$1");
        Dest.CopyMove(Self);
        MoveElements(ListA,Dest);
      } finally {
        Dest.$destroy("Destroy");
      };
    };
    this.DoOr = function (ListA, ListB) {
      if (ListB != null) {
        this.CopyMove(ListA);
        this.MergeMove(ListB);
      } else this.MergeMove(ListA);
    };
    this.DoXOr = function (ListA, ListB) {
      var r = 0;
      var l = null;
      if (ListB != null) {
        this.Clear();
        for (var $l1 = 0, $end2 = ListA.GetCount() - 1; $l1 <= $end2; $l1++) {
          r = $l1;
          if (ListB.IndexOf(ListA.Get(r)) < 0) this.Add(ListA.Get(r));
        };
        for (var $l3 = 0, $end4 = ListB.GetCount() - 1; $l3 <= $end4; $l3++) {
          r = $l3;
          if (ListA.IndexOf(ListB.Get(r)) < 0) this.Add(ListB.Get(r));
        };
      } else try {
        l = $mod.TList.$create("Create$1");
        l.CopyMove(this);
        for (var $l5 = this.GetCount() - 1; $l5 >= 0; $l5--) {
          r = $l5;
          if (ListA.IndexOf(this.Get(r)) >= 0) this.Delete(r);
        };
        for (var $l6 = 0, $end7 = ListA.GetCount() - 1; $l6 <= $end7; $l6++) {
          r = $l6;
          if (l.IndexOf(ListA.Get(r)) < 0) this.Add(ListA.Get(r));
        };
      } finally {
        l.$destroy("Destroy");
      };
    };
    this.Get = function (Index) {
      var Result = undefined;
      Result = this.FList.Get(Index);
      return Result;
    };
    this.Put = function (Index, Item) {
      var V = undefined;
      V = this.Get(Index);
      this.FList.Put(Index,Item);
      if (pas.System.Assigned(V)) this.Notify(V,$mod.TListNotification.lnDeleted);
      if (pas.System.Assigned(Item)) this.Notify(Item,$mod.TListNotification.lnAdded);
    };
    this.Notify = function (aValue, Action) {
      if (pas.System.Assigned(aValue)) ;
      if (Action === $mod.TListNotification.lnExtracted) ;
    };
    this.SetCapacity = function (NewCapacity) {
      this.FList.SetCapacity(NewCapacity);
    };
    this.GetCapacity = function () {
      var Result = 0;
      Result = this.FList.FCapacity;
      return Result;
    };
    this.SetCount = function (NewCount) {
      if (NewCount < this.FList.FCount) {
        while (this.FList.FCount > NewCount) this.Delete(this.FList.FCount - 1)}
       else this.FList.SetCount(NewCount);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FList.FCount;
      return Result;
    };
    this.GetList = function () {
      var Result = [];
      Result = this.FList.FList;
      return Result;
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FList = $mod.TFPList.$create("Create");
    };
    this.Destroy = function () {
      if (this.FList != null) this.Clear();
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FList;
        }, set: function (v) {
          this.p.FList = v;
        }});
    };
    this.AddList = function (AList) {
      var I = 0;
      this.FList.AddList(AList.FList);
      for (var $l1 = 0, $end2 = AList.GetCount() - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        if (pas.System.Assigned(AList.Get(I))) this.Notify(AList.Get(I),$mod.TListNotification.lnAdded);
      };
    };
    this.Add = function (Item) {
      var Result = 0;
      Result = this.FList.Add(Item);
      if (pas.System.Assigned(Item)) this.Notify(Item,$mod.TListNotification.lnAdded);
      return Result;
    };
    this.Clear = function () {
      while (this.FList.FCount > 0) this.Delete(this.GetCount() - 1);
    };
    this.Delete = function (Index) {
      var V = undefined;
      V = this.FList.Get(Index);
      this.FList.Delete(Index);
      if (pas.System.Assigned(V)) this.Notify(V,$mod.TListNotification.lnDeleted);
    };
    this.error = function (Msg, Data) {
      throw $mod.EListError.$create("CreateFmt",[Msg,[Data]]);
    };
    this.Exchange = function (Index1, Index2) {
      this.FList.Exchange(Index1,Index2);
    };
    this.Expand = function () {
      var Result = null;
      this.FList.Expand();
      Result = this;
      return Result;
    };
    this.Extract = function (Item) {
      var Result = undefined;
      var c = 0;
      c = this.FList.FCount;
      Result = this.FList.Extract(Item);
      if (c !== this.FList.FCount) this.Notify(Result,$mod.TListNotification.lnExtracted);
      return Result;
    };
    this.First = function () {
      var Result = undefined;
      Result = this.FList.First();
      return Result;
    };
    this.GetEnumerator = function () {
      var Result = null;
      Result = $mod.TListEnumerator.$create("Create$1",[this]);
      return Result;
    };
    this.IndexOf = function (Item) {
      var Result = 0;
      Result = this.FList.IndexOf(Item);
      return Result;
    };
    this.Insert = function (Index, Item) {
      this.FList.Insert(Index,Item);
      if (pas.System.Assigned(Item)) this.Notify(Item,$mod.TListNotification.lnAdded);
    };
    this.Last = function () {
      var Result = undefined;
      Result = this.FList.Last();
      return Result;
    };
    this.Move = function (CurIndex, NewIndex) {
      this.FList.Move(CurIndex,NewIndex);
    };
    this.Assign = function (ListA, AOperator, ListB) {
      var $tmp1 = AOperator;
      if ($tmp1 === $mod.TListAssignOp.laCopy) {
        this.DoCopy(ListA,ListB)}
       else if ($tmp1 === $mod.TListAssignOp.laSrcUnique) {
        this.DoSrcUnique(ListA,ListB)}
       else if ($tmp1 === $mod.TListAssignOp.laAnd) {
        this.DoAnd(ListA,ListB)}
       else if ($tmp1 === $mod.TListAssignOp.laDestUnique) {
        this.DoDestUnique(ListA,ListB)}
       else if ($tmp1 === $mod.TListAssignOp.laOr) {
        this.DoOr(ListA,ListB)}
       else if ($tmp1 === $mod.TListAssignOp.laXor) this.DoXOr(ListA,ListB);
    };
    this.Remove = function (Item) {
      var Result = 0;
      Result = this.IndexOf(Item);
      if (Result !== -1) this.Delete(Result);
      return Result;
    };
    this.Pack = function () {
      this.FList.Pack();
    };
    this.Sort = function (Compare) {
      this.FList.Sort(Compare);
    };
  });
  rtl.createClass($mod,"TPersistent",pas.System.TObject,function () {
    this.AssignError = function (Source) {
      var SourceName = "";
      if (Source !== null) {
        SourceName = Source.$classname}
       else SourceName = "Nil";
      throw pas.SysUtils.EConvertError.$create("Create$1",[((("Cannot assign a " + SourceName) + " to a ") + this.$classname) + "."]);
    };
    this.AssignTo = function (Dest) {
      Dest.AssignError(this);
    };
    this.GetOwner = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.Assign = function (Source) {
      if (Source !== null) {
        Source.AssignTo(this)}
       else this.AssignError(null);
    };
    this.GetNamePath = function () {
      var Result = "";
      var OwnerName = "";
      var TheOwner = null;
      Result = this.$classname;
      TheOwner = this.GetOwner();
      if (TheOwner !== null) {
        OwnerName = TheOwner.GetNamePath();
        if (OwnerName !== "") Result = (OwnerName + ".") + Result;
      };
      return Result;
    };
  });
  rtl.createClass($mod,"TStringsEnumerator",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FStrings = null;
      this.FPosition = 0;
    };
    this.$final = function () {
      this.FStrings = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (AStrings) {
      pas.System.TObject.Create.call(this);
      this.FStrings = AStrings;
      this.FPosition = -1;
    };
    this.GetCurrent = function () {
      var Result = "";
      Result = this.FStrings.Get(this.FPosition);
      return Result;
    };
    this.MoveNext = function () {
      var Result = false;
      this.FPosition += 1;
      Result = this.FPosition < this.FStrings.GetCount();
      return Result;
    };
  });
  rtl.createClass($mod,"TStrings",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FSpecialCharsInited = false;
      this.FAlwaysQuote = false;
      this.FQuoteChar = "";
      this.FDelimiter = "";
      this.FNameValueSeparator = "";
      this.FUpdateCount = 0;
      this.FLBS = 0;
      this.FSkipLastLineBreak = false;
      this.FStrictDelimiter = false;
      this.FLineBreak = "";
    };
    this.GetCommaText = function () {
      var Result = "";
      var C1 = "";
      var C2 = "";
      var FSD = false;
      this.CheckSpecialChars();
      FSD = this.FStrictDelimiter;
      C1 = this.GetDelimiter();
      C2 = this.GetQuoteChar();
      this.SetDelimiter(",");
      this.SetQuoteChar('"');
      this.FStrictDelimiter = false;
      try {
        Result = this.GetDelimitedText();
      } finally {
        this.SetDelimiter(C1);
        this.SetQuoteChar(C2);
        this.FStrictDelimiter = FSD;
      };
      return Result;
    };
    this.GetName = function (Index) {
      var Result = "";
      var V = "";
      this.GetNameValue(Index,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},{get: function () {
          return V;
        }, set: function (v) {
          V = v;
        }});
      return Result;
    };
    this.GetValue = function (Name) {
      var Result = "";
      var L = 0;
      var N = "";
      Result = "";
      L = this.IndexOfName(Name);
      if (L !== -1) this.GetNameValue(L,{get: function () {
          return N;
        }, set: function (v) {
          N = v;
        }},{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.GetLBS = function () {
      var Result = 0;
      this.CheckSpecialChars();
      Result = this.FLBS;
      return Result;
    };
    this.SetLBS = function (AValue) {
      this.CheckSpecialChars();
      this.FLBS = AValue;
    };
    this.SetCommaText = function (Value) {
      var C1 = "";
      var C2 = "";
      this.CheckSpecialChars();
      C1 = this.GetDelimiter();
      C2 = this.GetQuoteChar();
      this.SetDelimiter(",");
      this.SetQuoteChar('"');
      try {
        this.SetDelimitedText(Value);
      } finally {
        this.SetDelimiter(C1);
        this.SetQuoteChar(C2);
      };
    };
    this.SetValue = function (Name, Value) {
      var L = 0;
      this.CheckSpecialChars();
      L = this.IndexOfName(Name);
      if (L === -1) {
        this.Add((Name + this.FNameValueSeparator) + Value)}
       else this.Put(L,(Name + this.FNameValueSeparator) + Value);
    };
    this.SetDelimiter = function (c) {
      this.CheckSpecialChars();
      this.FDelimiter = c;
    };
    this.SetQuoteChar = function (c) {
      this.CheckSpecialChars();
      this.FQuoteChar = c;
    };
    this.SetNameValueSeparator = function (c) {
      this.CheckSpecialChars();
      this.FNameValueSeparator = c;
    };
    this.DoSetTextStr = function (Value, DoClear) {
      var S = "";
      var P = 0;
      try {
        this.BeginUpdate();
        if (DoClear) this.Clear();
        P = 1;
        while (this.GetNextLinebreak(Value,{get: function () {
            return S;
          }, set: function (v) {
            S = v;
          }},{get: function () {
            return P;
          }, set: function (v) {
            P = v;
          }})) this.Add(S);
      } finally {
        this.EndUpdate();
      };
    };
    this.GetDelimiter = function () {
      var Result = "";
      this.CheckSpecialChars();
      Result = this.FDelimiter;
      return Result;
    };
    this.GetNameValueSeparator = function () {
      var Result = "";
      this.CheckSpecialChars();
      Result = this.FNameValueSeparator;
      return Result;
    };
    this.GetQuoteChar = function () {
      var Result = "";
      this.CheckSpecialChars();
      Result = this.FQuoteChar;
      return Result;
    };
    this.GetLineBreak = function () {
      var Result = "";
      this.CheckSpecialChars();
      Result = this.FLineBreak;
      return Result;
    };
    this.SetLineBreak = function (S) {
      this.CheckSpecialChars();
      this.FLineBreak = S;
    };
    this.GetSkipLastLineBreak = function () {
      var Result = false;
      this.CheckSpecialChars();
      Result = this.FSkipLastLineBreak;
      return Result;
    };
    this.SetSkipLastLineBreak = function (AValue) {
      this.CheckSpecialChars();
      this.FSkipLastLineBreak = AValue;
    };
    this.error = function (Msg, Data) {
      throw $mod.EStringListError.$create("CreateFmt",[Msg,[pas.SysUtils.IntToStr(Data)]]);
    };
    this.GetCapacity = function () {
      var Result = 0;
      Result = this.GetCount();
      return Result;
    };
    this.GetObject = function (Index) {
      var Result = null;
      if (Index === 0) ;
      Result = null;
      return Result;
    };
    this.GetTextStr = function () {
      var Result = "";
      var I = 0;
      var S = "";
      var NL = "";
      this.CheckSpecialChars();
      if (this.FLineBreak !== pas.System.sLineBreak) {
        NL = this.FLineBreak}
       else {
        var $tmp1 = this.FLBS;
        if ($tmp1 === pas.System.TTextLineBreakStyle.tlbsLF) {
          NL = "\n"}
         else if ($tmp1 === pas.System.TTextLineBreakStyle.tlbsCRLF) {
          NL = "\r\n"}
         else if ($tmp1 === pas.System.TTextLineBreakStyle.tlbsCR) NL = "\r";
      };
      Result = "";
      for (var $l2 = 0, $end3 = this.GetCount() - 1; $l2 <= $end3; $l2++) {
        I = $l2;
        S = this.Get(I);
        Result = Result + S;
        if ((I < (this.GetCount() - 1)) || !this.GetSkipLastLineBreak()) Result = Result + NL;
      };
      return Result;
    };
    this.Put = function (Index, S) {
      var Obj = null;
      Obj = this.GetObject(Index);
      this.Delete(Index);
      this.InsertObject(Index,S,Obj);
    };
    this.PutObject = function (Index, AObject) {
      if (Index === 0) return;
      if (AObject === null) return;
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity === 0) ;
    };
    this.SetTextStr = function (Value) {
      this.CheckSpecialChars();
      this.DoSetTextStr(Value,true);
    };
    this.SetUpdateState = function (Updating) {
      if (Updating) ;
    };
    this.DoCompareText = function (s1, s2) {
      var Result = 0;
      Result = pas.SysUtils.CompareText(s1,s2);
      return Result;
    };
    this.GetDelimitedText = function () {
      var Result = "";
      var I = 0;
      var RE = "";
      var S = "";
      var doQuote = false;
      this.CheckSpecialChars();
      Result = "";
      RE = (this.GetQuoteChar() + "|") + this.GetDelimiter();
      if (!this.FStrictDelimiter) RE = " |" + RE;
      RE = ("\/" + RE) + "\/";
      for (var $l1 = 0, $end2 = this.GetCount() - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        S = this.Get(I);
        doQuote = this.FAlwaysQuote || (S.search(RE) === -1);
        if (doQuote) {
          Result = Result + pas.SysUtils.QuoteString(S,this.GetQuoteChar())}
         else Result = Result + S;
        if (I < (this.GetCount() - 1)) Result = Result + this.GetDelimiter();
      };
      if ((Result.length === 0) && (this.GetCount() === 1)) Result = this.GetQuoteChar() + this.GetQuoteChar();
      return Result;
    };
    this.SetDelimitedText = function (AValue) {
      var i = 0;
      var j = 0;
      var aNotFirst = false;
      this.CheckSpecialChars();
      this.BeginUpdate();
      i = 1;
      j = 1;
      aNotFirst = false;
      try {
        this.Clear();
        if (this.FStrictDelimiter) {
          while (i <= AValue.length) {
            if ((aNotFirst && (i <= AValue.length)) && (AValue.charAt(i - 1) === this.FDelimiter)) i += 1;
            if (i <= AValue.length) {
              if (AValue.charAt(i - 1) === this.FQuoteChar) {
                j = i + 1;
                while ((j <= AValue.length) && ((AValue.charAt(j - 1) !== this.FQuoteChar) || (((j + 1) <= AValue.length) && (AValue.charAt((j + 1) - 1) === this.FQuoteChar)))) {
                  if ((j <= AValue.length) && (AValue.charAt(j - 1) === this.FQuoteChar)) {
                    j += 2}
                   else j += 1;
                };
                this.Add(pas.SysUtils.StringReplace(pas.System.Copy(AValue,i + 1,(j - i) - 1),this.FQuoteChar + this.FQuoteChar,this.FQuoteChar,rtl.createSet(pas.SysUtils.TStringReplaceFlag.rfReplaceAll)));
                i = j + 1;
              } else {
                j = i;
                while ((j <= AValue.length) && (AValue.charAt(j - 1) !== this.FDelimiter)) j += 1;
                this.Add(pas.System.Copy(AValue,i,j - i));
                i = j;
              };
            } else {
              if (aNotFirst) this.Add("");
            };
            aNotFirst = true;
          };
        } else {
          while (i <= AValue.length) {
            if ((aNotFirst && (i <= AValue.length)) && (AValue.charAt(i - 1) === this.FDelimiter)) i += 1;
            while ((i <= AValue.length) && (AValue.charCodeAt(i - 1) <= " ".charCodeAt())) i += 1;
            if (i <= AValue.length) {
              if (AValue.charAt(i - 1) === this.FQuoteChar) {
                j = i + 1;
                while ((j <= AValue.length) && ((AValue.charAt(j - 1) !== this.FQuoteChar) || (((j + 1) <= AValue.length) && (AValue.charAt((j + 1) - 1) === this.FQuoteChar)))) {
                  if ((j <= AValue.length) && (AValue.charAt(j - 1) === this.FQuoteChar)) {
                    j += 2}
                   else j += 1;
                };
                this.Add(pas.SysUtils.StringReplace(pas.System.Copy(AValue,i + 1,(j - i) - 1),this.FQuoteChar + this.FQuoteChar,this.FQuoteChar,rtl.createSet(pas.SysUtils.TStringReplaceFlag.rfReplaceAll)));
                i = j + 1;
              } else {
                j = i;
                while (((j <= AValue.length) && (AValue.charCodeAt(j - 1) > " ".charCodeAt())) && (AValue.charAt(j - 1) !== this.FDelimiter)) j += 1;
                this.Add(pas.System.Copy(AValue,i,j - i));
                i = j;
              };
            } else {
              if (aNotFirst) this.Add("");
            };
            while ((i <= AValue.length) && (AValue.charCodeAt(i - 1) <= " ".charCodeAt())) i += 1;
            aNotFirst = true;
          };
        };
      } finally {
        this.EndUpdate();
      };
    };
    this.GetValueFromIndex = function (Index) {
      var Result = "";
      var N = "";
      this.GetNameValue(Index,{get: function () {
          return N;
        }, set: function (v) {
          N = v;
        }},{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.SetValueFromIndex = function (Index, Value) {
      if (Value === "") {
        this.Delete(Index)}
       else {
        if (Index < 0) Index = this.Add("");
        this.CheckSpecialChars();
        this.Put(Index,(this.GetName(Index) + this.FNameValueSeparator) + Value);
      };
    };
    this.CheckSpecialChars = function () {
      if (!this.FSpecialCharsInited) {
        this.FQuoteChar = '"';
        this.FDelimiter = ",";
        this.FNameValueSeparator = "=";
        this.FLBS = pas.System.DefaultTextLineBreakStyle;
        this.FSpecialCharsInited = true;
        this.FLineBreak = pas.System.sLineBreak;
      };
    };
    this.GetNextLinebreak = function (Value, S, P) {
      var Result = false;
      var PP = 0;
      S.set("");
      Result = false;
      if ((Value.length - P.get()) < 0) return Result;
      PP = Value.indexOf(this.GetLineBreak(),P.get() - 1) + 1;
      if (PP < 1) PP = Value.length + 1;
      S.set(pas.System.Copy(Value,P.get(),PP - P.get()));
      P.set(PP + this.GetLineBreak().length);
      Result = true;
      return Result;
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FAlwaysQuote = false;
    };
    this.Destroy = function () {
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function (S) {
      var Result = 0;
      Result = this.GetCount();
      this.Insert(this.GetCount(),S);
      return Result;
    };
    this.AddObject = function (S, AObject) {
      var Result = 0;
      Result = this.Add(S);
      this.PutObject(Result,AObject);
      return Result;
    };
    this.Append = function (S) {
      this.Add(S);
    };
    this.AddStrings = function (TheStrings) {
      var Runner = 0;
      for (var $l1 = 0, $end2 = TheStrings.GetCount() - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        this.AddObject(TheStrings.Get(Runner),TheStrings.GetObject(Runner));
      };
    };
    this.AddStrings$1 = function (TheStrings, ClearFirst) {
      this.BeginUpdate();
      try {
        if (ClearFirst) this.Clear();
        this.AddStrings(TheStrings);
      } finally {
        this.EndUpdate();
      };
    };
    this.AddStrings$2 = function (TheStrings) {
      var Runner = 0;
      if (((this.GetCount() + (rtl.length(TheStrings) - 1)) + 1) > this.GetCapacity()) this.SetCapacity((this.GetCount() + (rtl.length(TheStrings) - 1)) + 1);
      for (var $l1 = 0, $end2 = rtl.length(TheStrings) - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        this.Add(TheStrings[Runner]);
      };
    };
    this.AddStrings$3 = function (TheStrings, ClearFirst) {
      this.BeginUpdate();
      try {
        if (ClearFirst) this.Clear();
        this.AddStrings$2(TheStrings);
      } finally {
        this.EndUpdate();
      };
    };
    this.AddText = function (S) {
      this.CheckSpecialChars();
      this.DoSetTextStr(S,false);
    };
    this.Assign = function (Source) {
      var S = null;
      if ($mod.TStrings.isPrototypeOf(Source)) {
        S = Source;
        this.BeginUpdate();
        try {
          this.Clear();
          this.FSpecialCharsInited = S.FSpecialCharsInited;
          this.FQuoteChar = S.FQuoteChar;
          this.FDelimiter = S.FDelimiter;
          this.FNameValueSeparator = S.FNameValueSeparator;
          this.FLBS = S.FLBS;
          this.FLineBreak = S.FLineBreak;
          this.AddStrings(S);
        } finally {
          this.EndUpdate();
        };
      } else $mod.TPersistent.Assign.call(this,Source);
    };
    this.BeginUpdate = function () {
      if (this.FUpdateCount === 0) this.SetUpdateState(true);
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) this.FUpdateCount -= 1;
      if (this.FUpdateCount === 0) this.SetUpdateState(false);
    };
    this.Equals = function (Obj) {
      var Result = false;
      if ($mod.TStrings.isPrototypeOf(Obj)) {
        Result = this.Equals$2(Obj)}
       else Result = pas.System.TObject.Equals.call(this,Obj);
      return Result;
    };
    this.Equals$2 = function (TheStrings) {
      var Result = false;
      var Runner = 0;
      var Nr = 0;
      Result = false;
      Nr = this.GetCount();
      if (Nr !== TheStrings.GetCount()) return Result;
      for (var $l1 = 0, $end2 = Nr - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        if (this.Get(Runner) !== TheStrings.Get(Runner)) return Result;
      };
      Result = true;
      return Result;
    };
    this.Exchange = function (Index1, Index2) {
      var Obj = null;
      var Str = "";
      this.BeginUpdate();
      try {
        Obj = this.GetObject(Index1);
        Str = this.Get(Index1);
        this.PutObject(Index1,this.GetObject(Index2));
        this.Put(Index1,this.Get(Index2));
        this.PutObject(Index2,Obj);
        this.Put(Index2,Str);
      } finally {
        this.EndUpdate();
      };
    };
    this.GetEnumerator = function () {
      var Result = null;
      Result = $mod.TStringsEnumerator.$create("Create$1",[this]);
      return Result;
    };
    this.IndexOf = function (S) {
      var Result = 0;
      Result = 0;
      while ((Result < this.GetCount()) && (this.DoCompareText(this.Get(Result),S) !== 0)) Result = Result + 1;
      if (Result === this.GetCount()) Result = -1;
      return Result;
    };
    this.IndexOfName = function (Name) {
      var Result = 0;
      var len = 0;
      var S = "";
      this.CheckSpecialChars();
      Result = 0;
      while (Result < this.GetCount()) {
        S = this.Get(Result);
        len = pas.System.Pos(this.FNameValueSeparator,S) - 1;
        if ((len >= 0) && (this.DoCompareText(Name,pas.System.Copy(S,1,len)) === 0)) return Result;
        Result += 1;
      };
      Result = -1;
      return Result;
    };
    this.IndexOfObject = function (AObject) {
      var Result = 0;
      Result = 0;
      while ((Result < this.GetCount()) && (this.GetObject(Result) !== AObject)) Result = Result + 1;
      if (Result === this.GetCount()) Result = -1;
      return Result;
    };
    this.InsertObject = function (Index, S, AObject) {
      this.Insert(Index,S);
      this.PutObject(Index,AObject);
    };
    this.Move = function (CurIndex, NewIndex) {
      var Obj = null;
      var Str = "";
      this.BeginUpdate();
      try {
        Obj = this.GetObject(CurIndex);
        Str = this.Get(CurIndex);
        this.PutObject(CurIndex,null);
        this.Delete(CurIndex);
        this.InsertObject(NewIndex,Str,Obj);
      } finally {
        this.EndUpdate();
      };
    };
    this.GetNameValue = function (Index, AName, AValue) {
      var L = 0;
      this.CheckSpecialChars();
      AValue.set(this.Get(Index));
      L = pas.System.Pos(this.FNameValueSeparator,AValue.get());
      if (L !== 0) {
        AName.set(pas.System.Copy(AValue.get(),1,L - 1));
        AValue.set(pas.System.Copy(AValue.get(),L + 1,AValue.get().length - L));
      } else AName.set("");
    };
    this.ExtractName = function (S) {
      var Result = "";
      var L = 0;
      this.CheckSpecialChars();
      L = pas.System.Pos(this.FNameValueSeparator,S);
      if (L !== 0) {
        Result = pas.System.Copy(S,1,L - 1)}
       else Result = "";
      return Result;
    };
  });
  this.TStringItem = function (s) {
    if (s) {
      this.FString = s.FString;
      this.FObject = s.FObject;
    } else {
      this.FString = "";
      this.FObject = null;
    };
    this.$equal = function (b) {
      return (this.FString === b.FString) && (this.FObject === b.FObject);
    };
  };
  this.TStringsSortStyle = {"0": "sslNone", sslNone: 0, "1": "sslUser", sslUser: 1, "2": "sslAuto", sslAuto: 2};
  rtl.createClass($mod,"TStringList",$mod.TStrings,function () {
    this.$init = function () {
      $mod.TStrings.$init.call(this);
      this.FList = [];
      this.FCount = 0;
      this.FOnChange = null;
      this.FOnChanging = null;
      this.FDuplicates = 0;
      this.FCaseSensitive = false;
      this.FForceSort = false;
      this.FOwnsObjects = false;
      this.FSortStyle = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      this.FOnChange = undefined;
      this.FOnChanging = undefined;
      $mod.TStrings.$final.call(this);
    };
    this.ExchangeItemsInt = function (Index1, Index2) {
      var S = "";
      var O = null;
      S = this.FList[Index1].FString;
      O = this.FList[Index1].FObject;
      this.FList[Index1].FString = this.FList[Index2].FString;
      this.FList[Index1].FObject = this.FList[Index2].FObject;
      this.FList[Index2].FString = S;
      this.FList[Index2].FObject = O;
    };
    this.GetSorted = function () {
      var Result = false;
      Result = this.FSortStyle in rtl.createSet($mod.TStringsSortStyle.sslUser,$mod.TStringsSortStyle.sslAuto);
      return Result;
    };
    this.Grow = function () {
      var NC = 0;
      NC = this.GetCapacity();
      if (NC >= 256) {
        NC = NC + Math.floor(NC / 4)}
       else if (NC === 0) {
        NC = 4}
       else NC = NC * 4;
      this.SetCapacity(NC);
    };
    this.InternalClear = function (FromIndex, ClearOnly) {
      var I = 0;
      if (FromIndex < this.FCount) {
        if (this.FOwnsObjects) {
          for (var $l1 = FromIndex, $end2 = this.FCount - 1; $l1 <= $end2; $l1++) {
            I = $l1;
            this.FList[I].FString = "";
            pas.SysUtils.FreeAndNil({p: this.FList[I], get: function () {
                return this.p.FObject;
              }, set: function (v) {
                this.p.FObject = v;
              }});
          };
        } else {
          for (var $l3 = FromIndex, $end4 = this.FCount - 1; $l3 <= $end4; $l3++) {
            I = $l3;
            this.FList[I].FString = "";
          };
        };
        this.FCount = FromIndex;
      };
      if (!ClearOnly) this.SetCapacity(0);
    };
    this.QuickSort = function (L, R, CompareFn) {
      var Pivot = 0;
      var vL = 0;
      var vR = 0;
      if ((R - L) <= 1) {
        if (L < R) if (CompareFn(this,L,R) > 0) this.ExchangeItems(L,R);
        return;
      };
      vL = L;
      vR = R;
      Pivot = L + pas.System.Random(R - L);
      while (vL < vR) {
        while ((vL < Pivot) && (CompareFn(this,vL,Pivot) <= 0)) vL += 1;
        while ((vR > Pivot) && (CompareFn(this,vR,Pivot) > 0)) vR -= 1;
        this.ExchangeItems(vL,vR);
        if (Pivot === vL) {
          Pivot = vR}
         else if (Pivot === vR) Pivot = vL;
      };
      if ((Pivot - 1) >= L) this.QuickSort(L,Pivot - 1,CompareFn);
      if ((Pivot + 1) <= R) this.QuickSort(Pivot + 1,R,CompareFn);
    };
    this.SetSorted = function (Value) {
      if (Value) {
        this.SetSortStyle($mod.TStringsSortStyle.sslAuto)}
       else this.SetSortStyle($mod.TStringsSortStyle.sslNone);
    };
    this.SetCaseSensitive = function (b) {
      if (b === this.FCaseSensitive) return;
      this.FCaseSensitive = b;
      if (this.FSortStyle === $mod.TStringsSortStyle.sslAuto) {
        this.FForceSort = true;
        try {
          this.Sort();
        } finally {
          this.FForceSort = false;
        };
      };
    };
    this.SetSortStyle = function (AValue) {
      if (this.FSortStyle === AValue) return;
      if (AValue === $mod.TStringsSortStyle.sslAuto) this.Sort();
      this.FSortStyle = AValue;
    };
    this.CheckIndex = function (AIndex) {
      if ((AIndex < 0) || (AIndex >= this.FCount)) this.error(pas.RTLConsts.SListIndexError,AIndex);
    };
    this.ExchangeItems = function (Index1, Index2) {
      this.ExchangeItemsInt(Index1,Index2);
    };
    this.Changed = function () {
      if (this.FUpdateCount === 0) {
        if (this.FOnChange != null) this.FOnChange(this);
      };
    };
    this.Changing = function () {
      if (this.FUpdateCount === 0) if (this.FOnChanging != null) this.FOnChanging(this);
    };
    this.Get = function (Index) {
      var Result = "";
      this.CheckIndex(Index);
      Result = this.FList[Index].FString;
      return Result;
    };
    this.GetCapacity = function () {
      var Result = 0;
      Result = rtl.length(this.FList);
      return Result;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FCount;
      return Result;
    };
    this.GetObject = function (Index) {
      var Result = null;
      this.CheckIndex(Index);
      Result = this.FList[Index].FObject;
      return Result;
    };
    this.Put = function (Index, S) {
      if (this.GetSorted()) this.error(pas.RTLConsts.SSortedListError,0);
      this.CheckIndex(Index);
      this.Changing();
      this.FList[Index].FString = S;
      this.Changed();
    };
    this.PutObject = function (Index, AObject) {
      this.CheckIndex(Index);
      this.Changing();
      this.FList[Index].FObject = AObject;
      this.Changed();
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < 0) this.error(pas.RTLConsts.SListCapacityError,NewCapacity);
      if (NewCapacity !== this.GetCapacity()) this.FList = rtl.arraySetLength(this.FList,$mod.TStringItem,NewCapacity);
    };
    this.SetUpdateState = function (Updating) {
      if (Updating) {
        this.Changing()}
       else this.Changed();
    };
    this.InsertItem = function (Index, S) {
      this.InsertItem$1(Index,S,null);
    };
    this.InsertItem$1 = function (Index, S, O) {
      var It = new $mod.TStringItem();
      this.Changing();
      if (this.FCount === this.GetCapacity()) this.Grow();
      It.FString = S;
      It.FObject = O;
      this.FList.splice(Index,0,It);
      this.FCount += 1;
      this.Changed();
    };
    this.DoCompareText = function (s1, s2) {
      var Result = 0;
      if (this.FCaseSensitive) {
        Result = pas.SysUtils.CompareStr(s1,s2)}
       else Result = pas.SysUtils.CompareText(s1,s2);
      return Result;
    };
    this.CompareStrings = function (s1, s2) {
      var Result = 0;
      Result = this.DoCompareText(s1,s2);
      return Result;
    };
    this.Destroy = function () {
      this.InternalClear(0,false);
      $mod.TStrings.Destroy.call(this);
    };
    this.Add = function (S) {
      var Result = 0;
      if (!(this.FSortStyle === $mod.TStringsSortStyle.sslAuto)) {
        Result = this.FCount}
       else if (this.Find(S,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) {
        var $tmp1 = this.FDuplicates;
        if ($tmp1 === pas.Types.TDuplicates.dupIgnore) {
          return Result}
         else if ($tmp1 === pas.Types.TDuplicates.dupError) this.error(pas.RTLConsts.SDuplicateString,0);
      };
      this.InsertItem(Result,S);
      return Result;
    };
    this.Clear = function () {
      if (this.FCount === 0) return;
      this.Changing();
      this.InternalClear(0,false);
      this.Changed();
    };
    this.Delete = function (Index) {
      this.CheckIndex(Index);
      this.Changing();
      if (this.FOwnsObjects) pas.SysUtils.FreeAndNil({p: this.FList[Index], get: function () {
          return this.p.FObject;
        }, set: function (v) {
          this.p.FObject = v;
        }});
      this.FList.splice(Index,1);
      this.FList[this.GetCount() - 1].FString = "";
      this.FList[this.GetCount() - 1].FObject = null;
      this.FCount -= 1;
      this.Changed();
    };
    this.Exchange = function (Index1, Index2) {
      this.CheckIndex(Index1);
      this.CheckIndex(Index2);
      this.Changing();
      this.ExchangeItemsInt(Index1,Index2);
      this.Changed();
    };
    this.Find = function (S, Index) {
      var Result = false;
      var L = 0;
      var R = 0;
      var I = 0;
      var CompareRes = 0;
      Result = false;
      Index.set(-1);
      if (!this.GetSorted()) throw $mod.EListError.$create("Create$1",[pas.RTLConsts.SErrFindNeedsSortedList]);
      L = 0;
      R = this.GetCount() - 1;
      while (L <= R) {
        I = L + Math.floor((R - L) / 2);
        CompareRes = this.DoCompareText(S,this.FList[I].FString);
        if (CompareRes > 0) {
          L = I + 1}
         else {
          R = I - 1;
          if (CompareRes === 0) {
            Result = true;
            if (this.FDuplicates !== pas.Types.TDuplicates.dupAccept) L = I;
          };
        };
      };
      Index.set(L);
      return Result;
    };
    this.IndexOf = function (S) {
      var Result = 0;
      if (!this.GetSorted()) {
        Result = $mod.TStrings.IndexOf.call(this,S)}
       else if (!this.Find(S,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) Result = -1;
      return Result;
    };
    this.Insert = function (Index, S) {
      if (this.FSortStyle === $mod.TStringsSortStyle.sslAuto) {
        this.error(pas.RTLConsts.SSortedListError,0)}
       else {
        if ((Index < 0) || (Index > this.FCount)) this.error(pas.RTLConsts.SListIndexError,Index);
        this.InsertItem(Index,S);
      };
    };
    this.Sort = function () {
      this.CustomSort($impl.StringListAnsiCompare);
    };
    this.CustomSort = function (CompareFn) {
      if ((this.FForceSort || !(this.FSortStyle === $mod.TStringsSortStyle.sslAuto)) && (this.FCount > 1)) {
        this.Changing();
        this.QuickSort(0,this.FCount - 1,CompareFn);
        this.Changed();
      };
    };
  });
  rtl.createClass($mod,"TCollectionItem",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FCollection = null;
      this.FID = 0;
      this.FUpdateCount = 0;
    };
    this.$final = function () {
      this.FCollection = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.GetIndex = function () {
      var Result = 0;
      if (this.FCollection !== null) {
        Result = this.FCollection.FItems.IndexOf(this)}
       else Result = -1;
      return Result;
    };
    this.SetCollection = function (Value) {
      if (Value !== this.FCollection) {
        if (this.FCollection !== null) this.FCollection.RemoveItem(this);
        if (Value !== null) Value.InsertItem(this);
      };
    };
    this.Changed = function (AllItems) {
      if ((this.FCollection !== null) && (this.FCollection.FUpdateCount === 0)) {
        if (AllItems) {
          this.FCollection.Update(null)}
         else this.FCollection.Update(this);
      };
    };
    this.GetOwner = function () {
      var Result = null;
      Result = this.FCollection;
      return Result;
    };
    this.GetDisplayName = function () {
      var Result = "";
      Result = this.$classname;
      return Result;
    };
    this.SetIndex = function (Value) {
      var Temp = 0;
      Temp = this.GetIndex();
      if ((Temp > -1) && (Temp !== Value)) {
        this.FCollection.FItems.Move(Temp,Value);
        this.Changed(true);
      };
    };
    this.SetDisplayName = function (Value) {
      this.Changed(false);
      if (Value === "") ;
    };
    this.Create$1 = function (ACollection) {
      pas.System.TObject.Create.call(this);
      this.SetCollection(ACollection);
    };
    this.Destroy = function () {
      this.SetCollection(null);
      pas.System.TObject.Destroy.call(this);
    };
    this.GetNamePath = function () {
      var Result = "";
      if (this.FCollection !== null) {
        Result = ((this.FCollection.GetNamePath() + "[") + pas.SysUtils.IntToStr(this.GetIndex())) + "]"}
       else Result = this.$classname;
      return Result;
    };
  });
  rtl.createClass($mod,"TCollectionEnumerator",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FCollection = null;
      this.FPosition = 0;
    };
    this.$final = function () {
      this.FCollection = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (ACollection) {
      pas.System.TObject.Create.call(this);
      this.FCollection = ACollection;
      this.FPosition = -1;
    };
    this.GetCurrent = function () {
      var Result = null;
      Result = this.FCollection.GetItem(this.FPosition);
      return Result;
    };
    this.MoveNext = function () {
      var Result = false;
      this.FPosition += 1;
      Result = this.FPosition < this.FCollection.GetCount();
      return Result;
    };
  });
  this.TCollectionNotification = {"0": "cnAdded", cnAdded: 0, "1": "cnExtracting", cnExtracting: 1, "2": "cnDeleting", cnDeleting: 2};
  rtl.createClass($mod,"TCollection",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FItemClass = null;
      this.FItems = null;
      this.FUpdateCount = 0;
      this.FNextID = 0;
      this.FPropName = "";
    };
    this.$final = function () {
      this.FItemClass = undefined;
      this.FItems = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FItems.FCount;
      return Result;
    };
    this.GetPropName = function () {
      var Result = "";
      Result = this.FPropName;
      this.SetPropName();
      Result = this.FPropName;
      return Result;
    };
    this.InsertItem = function (Item) {
      if (!this.FItemClass.isPrototypeOf(Item)) return;
      this.FItems.Add(Item);
      Item.FCollection = this;
      Item.FID = this.FNextID;
      this.FNextID += 1;
      this.SetItemName(Item);
      this.Notify(Item,$mod.TCollectionNotification.cnAdded);
      this.Changed();
    };
    this.RemoveItem = function (Item) {
      var I = 0;
      this.Notify(Item,$mod.TCollectionNotification.cnExtracting);
      I = this.FItems.IndexOfItem(Item,pas.Types.TDirection.FromEnd);
      if (I !== -1) this.FItems.Delete(I);
      Item.FCollection = null;
      this.Changed();
    };
    this.DoClear = function () {
      var Item = null;
      while (this.FItems.FCount > 0) {
        Item = rtl.getObject(this.FItems.Last());
        if (Item != null) Item.$destroy("Destroy");
      };
    };
    this.GetAttrCount = function () {
      var Result = 0;
      Result = 0;
      return Result;
    };
    this.GetAttr = function (Index) {
      var Result = "";
      Result = "";
      if (Index === 0) ;
      return Result;
    };
    this.GetItemAttr = function (Index, ItemIndex) {
      var Result = "";
      Result = rtl.getObject(this.FItems.Get(ItemIndex)).GetDisplayName();
      if (Index === 0) ;
      return Result;
    };
    this.Changed = function () {
      if (this.FUpdateCount === 0) this.Update(null);
    };
    this.GetItem = function (Index) {
      var Result = null;
      Result = rtl.getObject(this.FItems.Get(Index));
      return Result;
    };
    this.SetItem = function (Index, Value) {
      rtl.getObject(this.FItems.Get(Index)).Assign(Value);
    };
    this.SetItemName = function (Item) {
      if (Item === null) ;
    };
    this.SetPropName = function () {
      this.FPropName = "";
    };
    this.Update = function (Item) {
      if (Item === null) ;
    };
    this.Notify = function (Item, Action) {
      if (Item === null) ;
      if (Action === $mod.TCollectionNotification.cnAdded) ;
    };
    this.Create$1 = function (AItemClass) {
      pas.System.TObject.Create.call(this);
      this.FItemClass = AItemClass;
      this.FItems = $mod.TFPList.$create("Create");
    };
    this.Destroy = function () {
      this.FUpdateCount = 1;
      try {
        this.DoClear();
      } finally {
        this.FUpdateCount = 0;
      };
      if (this.FItems != null) this.FItems.$destroy("Destroy");
      pas.System.TObject.Destroy.call(this);
    };
    this.Owner = function () {
      var Result = null;
      Result = this.GetOwner();
      return Result;
    };
    this.Add = function () {
      var Result = null;
      Result = this.FItemClass.$create("Create$1",[this]);
      return Result;
    };
    this.Assign = function (Source) {
      var I = 0;
      if ($mod.TCollection.isPrototypeOf(Source)) {
        this.Clear();
        for (var $l1 = 0, $end2 = Source.GetCount() - 1; $l1 <= $end2; $l1++) {
          I = $l1;
          this.Add().Assign(Source.GetItem(I));
        };
        return;
      } else $mod.TPersistent.Assign.call(this,Source);
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.Clear = function () {
      if (this.FItems.FCount === 0) return;
      this.BeginUpdate();
      try {
        this.DoClear();
      } finally {
        this.EndUpdate();
      };
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) this.FUpdateCount -= 1;
      if (this.FUpdateCount === 0) this.Changed();
    };
    this.Delete = function (Index) {
      var Item = null;
      Item = rtl.getObject(this.FItems.Get(Index));
      this.Notify(Item,$mod.TCollectionNotification.cnDeleting);
      if (Item != null) Item.$destroy("Destroy");
    };
    this.GetEnumerator = function () {
      var Result = null;
      Result = $mod.TCollectionEnumerator.$create("Create$1",[this]);
      return Result;
    };
    this.GetNamePath = function () {
      var Result = "";
      var o = null;
      o = this.GetOwner();
      if ((o != null) && (this.GetPropName() !== "")) {
        Result = (o.GetNamePath() + ".") + this.GetPropName()}
       else Result = this.$classname;
      return Result;
    };
    this.Insert = function (Index) {
      var Result = null;
      Result = this.Add();
      Result.SetIndex(Index);
      return Result;
    };
    this.FindItemID = function (ID) {
      var Result = null;
      var I = 0;
      for (var $l1 = 0, $end2 = this.FItems.FCount - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        Result = rtl.getObject(this.FItems.Get(I));
        if (Result.FID === ID) return Result;
      };
      Result = null;
      return Result;
    };
    this.Exchange = function (Index1, index2) {
      this.FItems.Exchange(Index1,index2);
    };
    this.Sort = function (Compare) {
      this.BeginUpdate();
      try {
        this.FItems.Sort(Compare);
      } finally {
        this.EndUpdate();
      };
    };
  });
  rtl.createClass($mod,"TOwnedCollection",$mod.TCollection,function () {
    this.$init = function () {
      $mod.TCollection.$init.call(this);
      this.FOwner = null;
    };
    this.$final = function () {
      this.FOwner = undefined;
      $mod.TCollection.$final.call(this);
    };
    this.GetOwner = function () {
      var Result = null;
      Result = this.FOwner;
      return Result;
    };
    this.Create$2 = function (AOwner, AItemClass) {
      this.FOwner = AOwner;
      $mod.TCollection.Create$1.call(this,AItemClass);
    };
  });
  this.TOperation = {"0": "opInsert", opInsert: 0, "1": "opRemove", opRemove: 1};
  this.TComponentStateItem = {"0": "csLoading", csLoading: 0, "1": "csReading", csReading: 1, "2": "csWriting", csWriting: 2, "3": "csDestroying", csDestroying: 3, "4": "csDesigning", csDesigning: 4, "5": "csAncestor", csAncestor: 5, "6": "csUpdating", csUpdating: 6, "7": "csFixups", csFixups: 7, "8": "csFreeNotification", csFreeNotification: 8, "9": "csInline", csInline: 9, "10": "csDesignInstance", csDesignInstance: 10};
  this.TComponentStyleItem = {"0": "csInheritable", csInheritable: 0, "1": "csCheckPropAvail", csCheckPropAvail: 1, "2": "csSubComponent", csSubComponent: 2, "3": "csTransient", csTransient: 3};
  rtl.createClass($mod,"TComponentEnumerator",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FComponent = null;
      this.FPosition = 0;
    };
    this.$final = function () {
      this.FComponent = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (AComponent) {
      pas.System.TObject.Create.call(this);
      this.FComponent = AComponent;
      this.FPosition = -1;
    };
    this.GetCurrent = function () {
      var Result = null;
      Result = this.FComponent.GetComponent(this.FPosition);
      return Result;
    };
    this.MoveNext = function () {
      var Result = false;
      this.FPosition += 1;
      Result = this.FPosition < this.FComponent.GetComponentCount();
      return Result;
    };
  });
  rtl.createClass($mod,"TComponent",$mod.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FOwner = null;
      this.FName = "";
      this.FTag = 0;
      this.FComponents = null;
      this.FFreeNotifies = null;
      this.FDesignInfo = 0;
      this.FComponentState = {};
      this.FComponentStyle = {};
    };
    this.$final = function () {
      this.FOwner = undefined;
      this.FComponents = undefined;
      this.FFreeNotifies = undefined;
      this.FComponentState = undefined;
      this.FComponentStyle = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.GetComponent = function (AIndex) {
      var Result = null;
      if (!(this.FComponents != null)) {
        Result = null}
       else Result = rtl.getObject(this.FComponents.Get(AIndex));
      return Result;
    };
    this.GetComponentCount = function () {
      var Result = 0;
      if (!(this.FComponents != null)) {
        Result = 0}
       else Result = this.FComponents.FCount;
      return Result;
    };
    this.GetComponentIndex = function () {
      var Result = 0;
      if ((this.FOwner != null) && (this.FOwner.FComponents != null)) {
        Result = this.FOwner.FComponents.IndexOf(this)}
       else Result = -1;
      return Result;
    };
    this.Insert = function (AComponent) {
      if (!(this.FComponents != null)) this.FComponents = $mod.TFPList.$create("Create");
      this.FComponents.Add(AComponent);
      AComponent.FOwner = this;
    };
    this.Remove = function (AComponent) {
      AComponent.FOwner = null;
      if (this.FComponents != null) {
        this.FComponents.Remove(AComponent);
        if (this.FComponents.FCount === 0) {
          this.FComponents.$destroy("Destroy");
          this.FComponents = null;
        };
      };
    };
    this.RemoveNotification = function (AComponent) {
      if (this.FFreeNotifies !== null) {
        this.FFreeNotifies.Remove(AComponent);
        if (this.FFreeNotifies.FCount === 0) {
          this.FFreeNotifies.$destroy("Destroy");
          this.FFreeNotifies = null;
          this.FComponentState = rtl.excludeSet(this.FComponentState,$mod.TComponentStateItem.csFreeNotification);
        };
      };
    };
    this.SetComponentIndex = function (Value) {
      var Temp = 0;
      var Count = 0;
      if (!(this.FOwner != null)) return;
      Temp = this.GetComponentIndex();
      if (Temp < 0) return;
      if (Value < 0) Value = 0;
      Count = this.FOwner.FComponents.FCount;
      if (Value >= Count) Value = Count - 1;
      if (Value !== Temp) {
        this.FOwner.FComponents.Delete(Temp);
        this.FOwner.FComponents.Insert(Value,this);
      };
    };
    this.ChangeName = function (NewName) {
      this.FName = NewName;
    };
    this.GetChildren = function (Proc, Root) {
      if (Proc === null) ;
      if (Root === null) ;
    };
    this.GetChildOwner = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.GetChildParent = function () {
      var Result = null;
      Result = this;
      return Result;
    };
    this.GetOwner = function () {
      var Result = null;
      Result = this.FOwner;
      return Result;
    };
    this.Loaded = function () {
      this.FComponentState = rtl.excludeSet(this.FComponentState,$mod.TComponentStateItem.csLoading);
    };
    this.Loading = function () {
      this.FComponentState = rtl.includeSet(this.FComponentState,$mod.TComponentStateItem.csLoading);
    };
    this.Notification = function (AComponent, Operation) {
      var C = 0;
      if (Operation === $mod.TOperation.opRemove) this.RemoveFreeNotification(AComponent);
      if (!(this.FComponents != null)) return;
      C = this.FComponents.FCount - 1;
      while (C >= 0) {
        rtl.getObject(this.FComponents.Get(C)).Notification(AComponent,Operation);
        C -= 1;
        if (C >= this.FComponents.FCount) C = this.FComponents.FCount - 1;
      };
    };
    this.PaletteCreated = function () {
    };
    this.SetAncestor = function (Value) {
      var Runner = 0;
      if (Value) {
        this.FComponentState = rtl.includeSet(this.FComponentState,$mod.TComponentStateItem.csAncestor)}
       else this.FComponentState = rtl.excludeSet(this.FComponentState,$mod.TComponentStateItem.csAncestor);
      if (this.FComponents != null) for (var $l1 = 0, $end2 = this.FComponents.FCount - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        rtl.getObject(this.FComponents.Get(Runner)).SetAncestor(Value);
      };
    };
    this.SetDesigning = function (Value, SetChildren) {
      var Runner = 0;
      if (Value) {
        this.FComponentState = rtl.includeSet(this.FComponentState,$mod.TComponentStateItem.csDesigning)}
       else this.FComponentState = rtl.excludeSet(this.FComponentState,$mod.TComponentStateItem.csDesigning);
      if ((this.FComponents != null) && SetChildren) for (var $l1 = 0, $end2 = this.FComponents.FCount - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        rtl.getObject(this.FComponents.Get(Runner)).SetDesigning(Value,true);
      };
    };
    this.SetDesignInstance = function (Value) {
      if (Value) {
        this.FComponentState = rtl.includeSet(this.FComponentState,$mod.TComponentStateItem.csDesignInstance)}
       else this.FComponentState = rtl.excludeSet(this.FComponentState,$mod.TComponentStateItem.csDesignInstance);
    };
    this.SetInline = function (Value) {
      if (Value) {
        this.FComponentState = rtl.includeSet(this.FComponentState,$mod.TComponentStateItem.csInline)}
       else this.FComponentState = rtl.excludeSet(this.FComponentState,$mod.TComponentStateItem.csInline);
    };
    this.SetName = function (NewName) {
      if (this.FName === NewName) return;
      if ((NewName !== "") && !pas.SysUtils.IsValidIdent(NewName,false,false)) throw $mod.EComponentError.$create("CreateFmt",[pas.RTLConsts.SInvalidName,[NewName]]);
      if (this.FOwner != null) {
        this.FOwner.ValidateRename(this,this.FName,NewName)}
       else this.ValidateRename(null,this.FName,NewName);
      this.ChangeName(NewName);
    };
    this.SetChildOrder = function (Child, Order) {
      if (Child === null) ;
      if (Order === 0) ;
    };
    this.SetParentComponent = function (Value) {
      if (Value === null) ;
    };
    this.Updating = function () {
      this.FComponentState = rtl.includeSet(this.FComponentState,$mod.TComponentStateItem.csUpdating);
    };
    this.Updated = function () {
      this.FComponentState = rtl.excludeSet(this.FComponentState,$mod.TComponentStateItem.csUpdating);
    };
    this.ValidateRename = function (AComponent, CurName, NewName) {
      if ((((AComponent !== null) && (pas.SysUtils.CompareText(CurName,NewName) !== 0)) && (AComponent.FOwner === this)) && (this.FindComponent(NewName) !== null)) throw $mod.EComponentError.$create("CreateFmt",[pas.RTLConsts.SDuplicateName,[NewName]]);
      if (($mod.TComponentStateItem.csDesigning in this.FComponentState) && (this.FOwner !== null)) this.FOwner.ValidateRename(AComponent,CurName,NewName);
    };
    this.ValidateContainer = function (AComponent) {
      AComponent.ValidateInsert(this);
    };
    this.ValidateInsert = function (AComponent) {
      if (AComponent === null) ;
    };
    this.Create$1 = function (AOwner) {
      this.FComponentStyle = rtl.createSet($mod.TComponentStyleItem.csInheritable);
      if (AOwner != null) AOwner.InsertComponent(this);
    };
    this.Destroy = function () {
      var I = 0;
      var C = null;
      this.Destroying();
      if (this.FFreeNotifies != null) {
        I = this.FFreeNotifies.FCount - 1;
        while (I >= 0) {
          C = rtl.getObject(this.FFreeNotifies.Get(I));
          this.FFreeNotifies.Delete(I);
          C.Notification(this,$mod.TOperation.opRemove);
          if (this.FFreeNotifies === null) {
            I = 0}
           else if (I > this.FFreeNotifies.FCount) I = this.FFreeNotifies.FCount;
          I -= 1;
        };
        pas.SysUtils.FreeAndNil({p: this, get: function () {
            return this.p.FFreeNotifies;
          }, set: function (v) {
            this.p.FFreeNotifies = v;
          }});
      };
      this.DestroyComponents();
      if (this.FOwner !== null) this.FOwner.RemoveComponent(this);
      pas.System.TObject.Destroy.call(this);
    };
    this.BeforeDestruction = function () {
      if (!($mod.TComponentStateItem.csDestroying in this.FComponentState)) this.Destroying();
    };
    this.DestroyComponents = function () {
      var acomponent = null;
      while (this.FComponents != null) {
        acomponent = rtl.getObject(this.FComponents.Last());
        this.Remove(acomponent);
        acomponent.$destroy("Destroy");
      };
    };
    this.Destroying = function () {
      var Runner = 0;
      if ($mod.TComponentStateItem.csDestroying in this.FComponentState) return;
      this.FComponentState = rtl.includeSet(this.FComponentState,$mod.TComponentStateItem.csDestroying);
      if (this.FComponents != null) for (var $l1 = 0, $end2 = this.FComponents.FCount - 1; $l1 <= $end2; $l1++) {
        Runner = $l1;
        rtl.getObject(this.FComponents.Get(Runner)).Destroying();
      };
    };
    this.FindComponent = function (AName) {
      var Result = null;
      var I = 0;
      Result = null;
      if ((AName === "") || !(this.FComponents != null)) return Result;
      for (var $l1 = 0, $end2 = this.FComponents.FCount - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        if (pas.SysUtils.CompareText(rtl.getObject(this.FComponents.Get(I)).FName,AName) === 0) {
          Result = rtl.getObject(this.FComponents.Get(I));
          return Result;
        };
      };
      return Result;
    };
    this.FreeNotification = function (AComponent) {
      if ((this.FOwner !== null) && (AComponent === this.FOwner)) return;
      if (!(this.FFreeNotifies != null)) this.FFreeNotifies = $mod.TFPList.$create("Create");
      if (this.FFreeNotifies.IndexOf(AComponent) === -1) {
        this.FFreeNotifies.Add(AComponent);
        AComponent.FreeNotification(this);
      };
    };
    this.RemoveFreeNotification = function (AComponent) {
      this.RemoveNotification(AComponent);
      AComponent.RemoveNotification(this);
    };
    this.GetNamePath = function () {
      var Result = "";
      Result = this.FName;
      return Result;
    };
    this.GetParentComponent = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.HasParent = function () {
      var Result = false;
      Result = false;
      return Result;
    };
    this.InsertComponent = function (AComponent) {
      AComponent.ValidateContainer(this);
      this.ValidateRename(AComponent,"",AComponent.FName);
      this.Insert(AComponent);
      if ($mod.TComponentStateItem.csDesigning in this.FComponentState) AComponent.SetDesigning(true,true);
      this.Notification(AComponent,$mod.TOperation.opInsert);
    };
    this.RemoveComponent = function (AComponent) {
      this.Notification(AComponent,$mod.TOperation.opRemove);
      this.Remove(AComponent);
      AComponent.SetDesigning(false,true);
      this.ValidateRename(AComponent,AComponent.FName,"");
    };
    this.SetSubComponent = function (ASubComponent) {
      if (ASubComponent) {
        this.FComponentStyle = rtl.includeSet(this.FComponentStyle,$mod.TComponentStyleItem.csSubComponent)}
       else this.FComponentStyle = rtl.excludeSet(this.FComponentStyle,$mod.TComponentStyleItem.csSubComponent);
    };
    this.GetEnumerator = function () {
      var Result = null;
      Result = $mod.TComponentEnumerator.$create("Create$1",[this]);
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Name",6,rtl.string,"FName","SetName");
    $r.addProperty("Tag",0,rtl.nativeint,"FTag","FTag");
  });
  this.RegisterClass = function (AClass) {
    $impl.ClassList[AClass.$classname] = AClass;
  };
  this.GetClass = function (AClassName) {
    var Result = null;
    Result = null;
    if (AClassName === "") return Result;
    if (!$impl.ClassList.hasOwnProperty(AClassName)) return Result;
    Result = rtl.getObject($impl.ClassList[AClassName]);
    return Result;
  };
  $mod.$init = function () {
    $impl.ClassList = Object.create(null);
  };
},["JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.QuickSort = function (aList, L, R, Compare) {
    var I = 0;
    var J = 0;
    var P = undefined;
    var Q = undefined;
    do {
      I = L;
      J = R;
      P = aList[Math.floor((L + R) / 2)];
      do {
        while (Compare(P,aList[I]) > 0) I = I + 1;
        while (Compare(P,aList[J]) < 0) J = J - 1;
        if (I <= J) {
          Q = aList[I];
          aList[I] = aList[J];
          aList[J] = Q;
          I = I + 1;
          J = J - 1;
        };
      } while (!(I > J));
      if ((J - L) < (R - I)) {
        if (L < J) $impl.QuickSort(aList,L,J,Compare);
        L = I;
      } else {
        if (I < R) $impl.QuickSort(aList,I,R,Compare);
        R = J;
      };
    } while (!(L >= R));
  };
  $impl.StringListAnsiCompare = function (List, Index1, Index) {
    var Result = 0;
    Result = List.DoCompareText(List.FList[Index1].FString,List.FList[Index].FString);
    return Result;
  };
  $impl.ClassList = null;
});
rtl.module("StringUtils",["System","Classes","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.FoundString = function (inString, searchString) {
    var Result = 0;
    var match = false;
    var found = false;
    var i = 0;
    var tempresult = 0;
    var tempstr = "";
    tempresult = 0;
    found = false;
    for (var $l1 = 1, $end2 = inString.length; $l1 <= $end2; $l1++) {
      i = $l1;
      if (found === false) {
        tempstr = inString.charAt(i - 1);
        if (tempstr === searchString.charAt(0)) {
          match = $impl.CheckMatch(inString,searchString,i);
          if (match === true) {
            tempresult = i;
            found = true;
          };
        };
      };
    };
    Result = tempresult;
    return Result;
  };
  this.myStringReplace = function (Instring, OldString, NewString, ReplaceNum, MaxStringLength) {
    var Result = "";
    var i = 0;
    var matchLength = 0;
    var match = false;
    var replaceCount = 0;
    var finalstring = "";
    var tempstr = "";
    finalstring = "";
    replaceCount = 0;
    matchLength = 0;
    for (var $l1 = 1, $end2 = Instring.length; $l1 <= $end2; $l1++) {
      i = $l1;
      tempstr = Instring.charAt(i - 1);
      if (((tempstr !== OldString.charAt(0)) && (i > matchLength)) || (i > MaxStringLength)) {
        finalstring = finalstring + tempstr;
      } else if (i > matchLength) {
        match = $impl.CheckMatch(Instring,OldString,i);
        if ((match === false) || (replaceCount >= ReplaceNum)) {
          finalstring = finalstring + tempstr}
         else {
          replaceCount = replaceCount + 1;
          finalstring = finalstring + NewString;
          matchLength = (i + OldString.length) - 1;
        };
      };
    };
    Result = finalstring;
    return Result;
  };
  this.MyBoolToStr = function (inBool) {
    var Result = "";
    if (inBool === true) {
      Result = "True"}
     else if (inBool === false) {
      Result = "False"}
     else $mod.ShowMessage("invalid boolean");
    return Result;
  };
  this.MyStrToBool = function (inStr) {
    var Result = false;
    if (pas.SysUtils.UpperCase($mod.TrimWhiteSpace(inStr)) === "TRUE") {
      Result = true}
     else if (pas.SysUtils.UpperCase($mod.TrimWhiteSpace(inStr)) === "FALSE") {
      Result = false}
     else $mod.ShowMessage("invalid boolean string " + inStr);
    return Result;
  };
  this.TrimWhiteSpace = function (Instring) {
    var Result = "";
    Result = $mod.DelChars(Instring," ");
    return Result;
  };
  this.stringsplit = function (str, separator) {
    var Result = null;
    var localStringList = null;
    localStringList = $mod.StringToSubStringList(str,separator);
    Result = localStringList;
    return Result;
  };
  this.IsInStringList = function (myList, elem) {
    var Result = false;
    var i = 0;
    var found = false;
    found = false;
    i = myList.IndexOf(elem);
    if (i > -1) found = true;
    Result = found;
    return Result;
  };
  this.ListStringToStringList = function (ListString) {
    var Result = null;
    var items = null;
    var TempString = "";
    TempString = ListString;
    TempString = pas.SysUtils.StringReplace(TempString,"[","",rtl.createSet(pas.SysUtils.TStringReplaceFlag.rfReplaceAll));
    TempString = pas.SysUtils.StringReplace(TempString,"]","",rtl.createSet(pas.SysUtils.TStringReplaceFlag.rfReplaceAll));
    TempString = pas.SysUtils.StringReplace(TempString,'"',"",rtl.createSet(pas.SysUtils.TStringReplaceFlag.rfReplaceAll));
    items = pas.Classes.TStringList.$create("Create$1");
    items.FStrictDelimiter = true;
    items.SetLineBreak(",");
    items.SetTextStr(TempString);
    Result = items;
    return Result;
  };
  this.StringToSubStringList = function (InString, delimiter) {
    var Result = null;
    var items = null;
    items = pas.Classes.TStringList.$create("Create$1");
    items.FStrictDelimiter = true;
    items.SetLineBreak(delimiter);
    items.SetTextStr(InString);
    Result = items;
    return Result;
  };
  this.DelChars = function (Instring, FilterChar) {
    var Result = "";
    var i = 0;
    var newstring = "";
    var tempstr = "";
    newstring = "";
    for (var $l1 = 1, $end2 = Instring.length; $l1 <= $end2; $l1++) {
      i = $l1;
      tempstr = Instring.charAt(i - 1);
      if (tempstr !== FilterChar) {
        newstring = newstring + tempstr;
      };
    };
    Result = newstring;
    return Result;
  };
  this.stripLeadingStringIfPresent = function (instring, LeadingString) {
    var Result = "";
    var i = 0;
    var OutString = "";
    var done = false;
    OutString = "";
    done = false;
    for (var $l1 = 1, $end2 = instring.length; $l1 <= $end2; $l1++) {
      i = $l1;
      if ((i <= LeadingString.length) && (done === false)) {
        if (instring.charAt(i - 1) !== LeadingString.charAt(i - 1)) {
          done = true;
          OutString = OutString + instring.charAt(i - 1);
        };
      } else OutString = OutString + instring.charAt(i - 1);
    };
    Result = OutString;
    return Result;
  };
  this.ShowMessage = function (text) {
    alert(text);
  };
  this.MainUnitName = "";
  $mod.$init = function () {
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.CheckMatch = function (Instring, teststring, startpos) {
    var Result = false;
    var i = 0;
    var match = false;
    var temp1 = "";
    var temp2 = "";
    match = true;
    for (var $l1 = 1, $end2 = teststring.length; $l1 <= $end2; $l1++) {
      i = $l1;
      if (((i + startpos) - 1) <= Instring.length) {
        temp1 = Instring.charAt(((i + startpos) - 1) - 1);
        temp2 = teststring.charAt(i - 1);
        if (temp1 !== temp2) match = false;
      } else match = false;
    };
    Result = match;
    return Result;
  };
});
rtl.module("TypInfo",["System","SysUtils","Types","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TTypeKind = {"0": "tkUnknown", tkUnknown: 0, "1": "tkInteger", tkInteger: 1, "2": "tkChar", tkChar: 2, "3": "tkString", tkString: 3, "4": "tkEnumeration", tkEnumeration: 4, "5": "tkSet", tkSet: 5, "6": "tkDouble", tkDouble: 6, "7": "tkBool", tkBool: 7, "8": "tkProcVar", tkProcVar: 8, "9": "tkMethod", tkMethod: 9, "10": "tkArray", tkArray: 10, "11": "tkDynArray", tkDynArray: 11, "12": "tkRecord", tkRecord: 12, "13": "tkClass", tkClass: 13, "14": "tkClassRef", tkClassRef: 14, "15": "tkPointer", tkPointer: 15, "16": "tkJSValue", tkJSValue: 16, "17": "tkRefToProcVar", tkRefToProcVar: 17};
  this.tkFloat = $mod.TTypeKind.tkDouble;
  this.tkProcedure = $mod.TTypeKind.tkProcVar;
  this.tkAny = rtl.createSet(null,$mod.TTypeKind.tkUnknown,$mod.TTypeKind.tkRefToProcVar);
  this.tkMethods = rtl.createSet($mod.TTypeKind.tkMethod);
  this.tkProperties = rtl.diffSet(rtl.diffSet($mod.tkAny,$mod.tkMethods),rtl.createSet($mod.TTypeKind.tkUnknown));
  this.TOrdType = {"0": "otSByte", otSByte: 0, "1": "otUByte", otUByte: 1, "2": "otSWord", otSWord: 2, "3": "otUWord", otUWord: 3, "4": "otSLong", otSLong: 4, "5": "otULong", otULong: 5, "6": "otSIntDouble", otSIntDouble: 6, "7": "otUIntDouble", otUIntDouble: 7};
  this.TParamFlag = {"0": "pfVar", pfVar: 0, "1": "pfConst", pfConst: 1, "2": "pfOut", pfOut: 2, "3": "pfArray", pfArray: 3};
  this.TProcedureFlag = {"0": "pfStatic", pfStatic: 0, "1": "pfVarargs", pfVarargs: 1, "2": "pfExternal", pfExternal: 2};
  this.TMethodKind = {"0": "mkProcedure", mkProcedure: 0, "1": "mkFunction", mkFunction: 1, "2": "mkConstructor", mkConstructor: 2, "3": "mkDestructor", mkDestructor: 3, "4": "mkClassProcedure", mkClassProcedure: 4, "5": "mkClassFunction", mkClassFunction: 5};
  this.TTypeMemberKind = {"0": "tmkUnknown", tmkUnknown: 0, "1": "tmkField", tmkField: 1, "2": "tmkMethod", tmkMethod: 2, "3": "tmkProperty", tmkProperty: 3};
  this.pfGetFunction = 1;
  this.pfSetProcedure = 2;
  this.pfStoredFalse = 4;
  this.pfStoredField = 8;
  this.pfStoredFunction = 12;
  this.pfHasIndex = 16;
  rtl.createClass($mod,"EPropertyError",pas.SysUtils.Exception,function () {
  });
  this.GetClassMembers = function (aTIClass) {
    var Result = [];
    var C = null;
    var i = 0;
    var Cnt = 0;
    var j = 0;
    Cnt = 0;
    C = aTIClass;
    while (C !== null) {
      Cnt += rtl.length(C.names);
      C = C.ancestor;
    };
    Result = rtl.arraySetLength(Result,null,Cnt);
    C = aTIClass;
    i = 0;
    while (C !== null) {
      for (var $l1 = 0, $end2 = rtl.length(C.names) - 1; $l1 <= $end2; $l1++) {
        j = $l1;
        Result[i] = C.members[C.names[j]];
        i += 1;
      };
      C = C.ancestor;
    };
    return Result;
  };
  this.GetClassMember = function (aTIClass, aName) {
    var Result = null;
    var C = null;
    var i = 0;
    C = aTIClass;
    while (C !== null) {
      if (C.members.hasOwnProperty(aName)) return C.members[aName];
      C = C.ancestor;
    };
    C = aTIClass;
    while (C !== null) {
      for (var $l1 = 0, $end2 = rtl.length(C.names) - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        if (pas.SysUtils.CompareText(C.names[i],aName) === 0) return C.members[C.names[i]];
      };
      C = C.ancestor;
    };
    Result = null;
    return Result;
  };
  this.GetInstanceMethod = function (Instance, aName) {
    var Result = null;
    var TI = null;
    if (Instance === null) return null;
    TI = $mod.GetClassMember(Instance.$rtti,aName);
    if (!rtl.isExt(TI,rtl.tTypeMemberMethod)) return null;
    Result = rtl.createCallback(Instance,TI.name);
    return Result;
  };
  this.GetClassMethods = function (aTIClass) {
    var Result = [];
    var C = null;
    var i = 0;
    var Cnt = 0;
    var j = 0;
    Cnt = 0;
    C = aTIClass;
    while (C !== null) {
      Cnt += C.methods.length;
      C = C.ancestor;
    };
    Result = rtl.arraySetLength(Result,null,Cnt);
    C = aTIClass;
    i = 0;
    while (C !== null) {
      for (var $l1 = 0, $end2 = C.methods.length - 1; $l1 <= $end2; $l1++) {
        j = $l1;
        Result[i] = C.members[C.methods[j]];
        i += 1;
      };
      C = C.ancestor;
    };
    return Result;
  };
  this.GetPropInfos = function (aTIClass) {
    var Result = [];
    var C = null;
    var i = 0;
    var Cnt = 0;
    var j = 0;
    Cnt = 0;
    C = aTIClass;
    while (C !== null) {
      Cnt += C.properties.length;
      C = C.ancestor;
    };
    Result = rtl.arraySetLength(Result,null,Cnt);
    C = aTIClass;
    i = 0;
    while (C !== null) {
      for (var $l1 = 0, $end2 = C.properties.length - 1; $l1 <= $end2; $l1++) {
        j = $l1;
        Result[i] = C.members[C.properties[j]];
        i += 1;
      };
      C = C.ancestor;
    };
    return Result;
  };
  this.GetPropInfo = function (TI, PropName) {
    var Result = null;
    var m = null;
    var i = 0;
    var C = null;
    C = TI;
    while (C !== null) {
      m = C.members[PropName];
      if (rtl.isExt(m,rtl.tTypeMemberProperty)) return m;
      C = C.ancestor;
    };
    Result = null;
    do {
      for (var $l1 = 0, $end2 = TI.properties.length - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        if (pas.SysUtils.CompareText(PropName,TI.properties[i]) === 0) {
          m = TI.members[TI.properties[i]];
          if (rtl.isExt(m,rtl.tTypeMemberProperty)) Result = m;
          return Result;
        };
      };
      TI = TI.ancestor;
    } while (!(TI === null));
    return Result;
  };
  this.GetPropInfo$1 = function (TI, PropName, Kinds) {
    var Result = null;
    Result = $mod.GetPropInfo(TI,PropName);
    if ((rtl.neSet(Kinds,{}) && (Result !== null)) && !(Result.typeinfo.kind in Kinds)) Result = null;
    return Result;
  };
  this.GetPropInfo$2 = function (Instance, PropName) {
    var Result = null;
    Result = $mod.GetPropInfo$1(Instance.$rtti,PropName,{});
    return Result;
  };
  this.GetPropInfo$3 = function (Instance, PropName, Kinds) {
    var Result = null;
    Result = $mod.GetPropInfo$1(Instance.$rtti,PropName,Kinds);
    return Result;
  };
  this.GetPropInfo$4 = function (aClass, PropName) {
    var Result = null;
    Result = $mod.GetPropInfo$1(aClass.$rtti,PropName,{});
    return Result;
  };
  this.GetPropInfo$5 = function (aClass, PropName, Kinds) {
    var Result = null;
    Result = $mod.GetPropInfo$1(aClass.$rtti,PropName,Kinds);
    return Result;
  };
  this.FindPropInfo = function (Instance, PropName) {
    var Result = null;
    Result = $mod.GetPropInfo(Instance.$rtti,PropName);
    if (Result === null) throw $mod.EPropertyError.$create("CreateFmt",[pas.RTLConsts.SErrPropertyNotFound,[PropName]]);
    return Result;
  };
  this.FindPropInfo$1 = function (Instance, PropName, Kinds) {
    var Result = null;
    Result = $mod.GetPropInfo$1(Instance.$rtti,PropName,Kinds);
    if (Result === null) throw $mod.EPropertyError.$create("CreateFmt",[pas.RTLConsts.SErrPropertyNotFound,[PropName]]);
    return Result;
  };
  this.FindPropInfo$2 = function (aClass, PropName) {
    var Result = null;
    Result = $mod.GetPropInfo(aClass.$rtti,PropName);
    if (Result === null) throw $mod.EPropertyError.$create("CreateFmt",[pas.RTLConsts.SErrPropertyNotFound,[PropName]]);
    return Result;
  };
  this.FindPropInfo$3 = function (aClass, PropName, Kinds) {
    var Result = null;
    Result = $mod.GetPropInfo$1(aClass.$rtti,PropName,Kinds);
    if (Result === null) throw $mod.EPropertyError.$create("CreateFmt",[pas.RTLConsts.SErrPropertyNotFound,[PropName]]);
    return Result;
  };
  this.IsStoredProp = function (Instance, PropInfo) {
    var Result = false;
    var $tmp1 = PropInfo.flags & 12;
    if ($tmp1 === 0) {
      Result = true}
     else if ($tmp1 === 4) {
      Result = false}
     else if ($tmp1 === 8) {
      Result = !(Instance[PropInfo.stored] == false)}
     else {
      Result = Instance[PropInfo.stored]();
    };
    return Result;
  };
  this.IsStoredProp$1 = function (Instance, PropName) {
    var Result = false;
    Result = $mod.IsStoredProp(Instance,$mod.FindPropInfo(Instance,PropName));
    return Result;
  };
  this.IsPublishedProp = function (Instance, PropName) {
    var Result = false;
    Result = $mod.GetPropInfo$2(Instance,PropName) !== null;
    return Result;
  };
  this.IsPublishedProp$1 = function (aClass, PropName) {
    var Result = false;
    Result = $mod.GetPropInfo$4(aClass,PropName) !== null;
    return Result;
  };
  this.PropType = function (Instance, PropName) {
    var Result = 0;
    Result = $mod.FindPropInfo(Instance,PropName).typeinfo.kind;
    return Result;
  };
  this.PropType$1 = function (aClass, PropName) {
    var Result = 0;
    Result = $mod.FindPropInfo$2(aClass,PropName).typeinfo.kind;
    return Result;
  };
  this.PropIsType = function (Instance, PropName, TypeKind) {
    var Result = false;
    Result = $mod.PropType(Instance,PropName) === TypeKind;
    return Result;
  };
  this.PropIsType$1 = function (aClass, PropName, TypeKind) {
    var Result = false;
    Result = $mod.PropType$1(aClass,PropName) === TypeKind;
    return Result;
  };
  this.GetJSValueProp = function (Instance, PropName) {
    var Result = undefined;
    Result = $mod.GetJSValueProp$1(Instance,$mod.FindPropInfo(Instance,PropName));
    return Result;
  };
  this.GetJSValueProp$1 = function (Instance, PropInfo) {
    var Result = undefined;
    var gk = 0;
    gk = $impl.GetPropGetterKind(PropInfo);
    var $tmp1 = gk;
    if ($tmp1 === $impl.TGetterKind.gkNone) {
      throw $mod.EPropertyError.$create("CreateFmt",[pas.RTLConsts.SCantReadPropertyS,[PropInfo.name]])}
     else if ($tmp1 === $impl.TGetterKind.gkField) {
      Result = Instance[PropInfo.getter]}
     else if ($tmp1 === $impl.TGetterKind.gkFunction) {
      if ((16 & PropInfo.flags) > 0) {
        Result = Instance[PropInfo.getter](PropInfo.index)}
       else Result = Instance[PropInfo.getter]()}
     else if ($tmp1 === $impl.TGetterKind.gkFunctionWithParams) throw $mod.EPropertyError.$create("CreateFmt",[pas.RTLConsts.SIndexedPropertyNeedsParams,[PropInfo.name]]);
    return Result;
  };
  this.SetJSValueProp = function (Instance, PropName, Value) {
    $mod.SetJSValueProp$1(Instance,$mod.FindPropInfo(Instance,PropName),Value);
  };
  this.SetJSValueProp$1 = function (Instance, PropInfo, Value) {
    var sk = 0;
    sk = $impl.GetPropSetterKind(PropInfo);
    var $tmp1 = sk;
    if ($tmp1 === $impl.TSetterKind.skNone) {
      throw $mod.EPropertyError.$create("CreateFmt",[pas.RTLConsts.SCantWritePropertyS,[PropInfo.name]])}
     else if ($tmp1 === $impl.TSetterKind.skField) {
      Instance[PropInfo.setter] = Value}
     else if ($tmp1 === $impl.TSetterKind.skProcedure) {
      if ((16 & PropInfo.flags) > 0) {
        Instance[PropInfo.setter](PropInfo.index,Value)}
       else Instance[PropInfo.setter](Value)}
     else if ($tmp1 === $impl.TSetterKind.skProcedureWithParams) throw $mod.EPropertyError.$create("CreateFmt",[pas.RTLConsts.SIndexedPropertyNeedsParams,[PropInfo.name]]);
  };
  this.GetNativeIntProp = function (Instance, PropName) {
    var Result = 0;
    Result = $mod.GetNativeIntProp$1(Instance,$mod.FindPropInfo(Instance,PropName));
    return Result;
  };
  this.GetNativeIntProp$1 = function (Instance, PropInfo) {
    var Result = 0;
    Result = Math.floor($mod.GetJSValueProp$1(Instance,PropInfo));
    return Result;
  };
  this.SetNativeIntProp = function (Instance, PropName, Value) {
    $mod.SetJSValueProp$1(Instance,$mod.FindPropInfo(Instance,PropName),Value);
  };
  this.SetNativeIntProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$1(Instance,PropInfo,Value);
  };
  this.GetStringProp = function (Instance, PropName) {
    var Result = "";
    Result = $mod.GetStringProp$1(Instance,$mod.FindPropInfo(Instance,PropName));
    return Result;
  };
  this.GetStringProp$1 = function (Instance, PropInfo) {
    var Result = "";
    Result = "" + $mod.GetJSValueProp$1(Instance,PropInfo);
    return Result;
  };
  this.SetStringProp = function (Instance, PropName, Value) {
    $mod.SetStringProp$1(Instance,$mod.FindPropInfo(Instance,PropName),Value);
  };
  this.SetStringProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$1(Instance,PropInfo,Value);
  };
  this.GetBoolProp = function (Instance, PropName) {
    var Result = false;
    Result = $mod.GetBoolProp$1(Instance,$mod.FindPropInfo(Instance,PropName));
    return Result;
  };
  this.GetBoolProp$1 = function (Instance, PropInfo) {
    var Result = false;
    Result = !($mod.GetJSValueProp$1(Instance,PropInfo) == false);
    return Result;
  };
  this.SetBoolProp = function (Instance, PropName, Value) {
    $mod.SetBoolProp$1(Instance,$mod.FindPropInfo(Instance,PropName),Value);
  };
  this.SetBoolProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$1(Instance,PropInfo,Value);
  };
  this.GetObjectProp = function (Instance, PropName) {
    var Result = null;
    Result = $mod.GetObjectProp$2(Instance,$mod.FindPropInfo(Instance,PropName));
    return Result;
  };
  this.GetObjectProp$1 = function (Instance, PropName, MinClass) {
    var Result = null;
    Result = $mod.GetObjectProp$2(Instance,$mod.FindPropInfo(Instance,PropName));
    if ((MinClass !== null) && (Result !== null)) if (!Result.$class.InheritsFrom(MinClass)) Result = null;
    return Result;
  };
  this.GetObjectProp$2 = function (Instance, PropInfo) {
    var Result = null;
    Result = $mod.GetObjectProp$3(Instance,PropInfo,null);
    return Result;
  };
  this.GetObjectProp$3 = function (Instance, PropInfo, MinClass) {
    var Result = null;
    var O = null;
    O = rtl.getObject($mod.GetJSValueProp$1(Instance,PropInfo));
    if ((MinClass !== null) && !O.$class.InheritsFrom(MinClass)) {
      Result = null}
     else Result = O;
    return Result;
  };
  this.SetObjectProp = function (Instance, PropName, Value) {
    $mod.SetObjectProp$1(Instance,$mod.FindPropInfo(Instance,PropName),Value);
  };
  this.SetObjectProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$1(Instance,PropInfo,Value);
  };
  this.GetFloatProp = function (Instance, PropName) {
    var Result = 0.0;
    Result = $mod.GetFloatProp$1(Instance,$mod.FindPropInfo(Instance,PropName));
    return Result;
  };
  this.GetFloatProp$1 = function (Instance, PropInfo) {
    var Result = 0.0;
    Result = rtl.getNumber($mod.GetJSValueProp$1(Instance,PropInfo));
    return Result;
  };
  this.SetFloatProp = function (Instance, PropName, Value) {
    $mod.SetFloatProp$1(Instance,$mod.FindPropInfo(Instance,PropName),Value);
  };
  this.SetFloatProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$1(Instance,PropInfo,Value);
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.TGetterKind = {"0": "gkNone", gkNone: 0, "1": "gkField", gkField: 1, "2": "gkFunction", gkFunction: 2, "3": "gkFunctionWithParams", gkFunctionWithParams: 3};
  $impl.GetPropGetterKind = function (PropInfo) {
    var Result = 0;
    if (PropInfo.getter === "") {
      Result = $impl.TGetterKind.gkNone}
     else if ((1 & PropInfo.flags) > 0) {
      if (rtl.length(PropInfo.params) > 0) {
        Result = $impl.TGetterKind.gkFunctionWithParams}
       else Result = $impl.TGetterKind.gkFunction;
    } else Result = $impl.TGetterKind.gkField;
    return Result;
  };
  $impl.TSetterKind = {"0": "skNone", skNone: 0, "1": "skField", skField: 1, "2": "skProcedure", skProcedure: 2, "3": "skProcedureWithParams", skProcedureWithParams: 3};
  $impl.GetPropSetterKind = function (PropInfo) {
    var Result = 0;
    if (PropInfo.setter === "") {
      Result = $impl.TSetterKind.skNone}
     else if ((2 & PropInfo.flags) > 0) {
      if (rtl.length(PropInfo.params) > 0) {
        Result = $impl.TSetterKind.skProcedureWithParams}
       else Result = $impl.TSetterKind.skProcedure;
    } else Result = $impl.TSetterKind.skField;
    return Result;
  };
});
rtl.module("Popup",["System","Classes","SysUtils","NodeUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.ShowPopup = function (popupID, modal) {
    $mod.OpenModal(popupID);
  };
  rtl.createClass($mod,"TXPopup",pas.NodeUtils.TInterfaceObject,function () {
    this.$init = function () {
      pas.NodeUtils.TInterfaceObject.$init.call(this);
      this.fIsSelected = false;
      this.fIsContainer = false;
    };
    this.getHeight = function () {
      var Result = "";
      Result = this.GetAttribute("Height",true).AttribValue;
      return Result;
    };
    this.getWidth = function () {
      var Result = "";
      Result = this.GetAttribute("Width",true).AttribValue;
      return Result;
    };
    this.getTop = function () {
      var Result = 0;
      var AttrVal = "";
      AttrVal = this.GetAttribute("Top",true).AttribValue;
      if (AttrVal !== "") {
        Result = pas.SysUtils.StrToInt(AttrVal)}
       else Result = 0;
      return Result;
    };
    this.getLeft = function () {
      var Result = 0;
      var AttrVal = "";
      AttrVal = this.GetAttribute("Left",true).AttribValue;
      if (AttrVal !== "") {
        Result = pas.SysUtils.StrToInt(AttrVal)}
       else Result = 0;
      return Result;
    };
    this.getCaption = function () {
      var Result = "";
      Result = this.GetAttribute("Caption",true).AttribValue;
      return Result;
    };
    this.SetHeight = function (AValue) {
      this.SetAttributeValue$1("Height",AValue);
      var ob=document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
        pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
      };
    };
    this.SetWidth = function (AValue) {
      this.SetAttributeValue$1("Width",AValue);
      var ob=document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
        pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
      };
    };
    this.SetTop = function (AValue) {
      var t = "";
      this.SetAttributeValue$1("Top",pas.SysUtils.IntToStr(AValue));
      t = pas.SysUtils.IntToStr(AValue) + "px";
      var ob=document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
        ob.style.top=t;
      };
    };
    this.SetLeft = function (AValue) {
      var l = "";
      this.SetAttributeValue$1("Left",pas.SysUtils.IntToStr(AValue));
      l = pas.SysUtils.IntToStr(AValue) + "px";
      var ob=document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
        ob.style.left=l;
      };
    };
    this.SetCaption = function (AValue) {
      this.SetAttributeValue$1("Caption",AValue);
      var ob=document.getElementById(this.NodeName+'Caption');
      if (ob!=null) {
        ob.innerHTML=AValue;
      };
    };
    var $r = this.$rtti;
    $r.addProperty("IsContainer",0,rtl.boolean,"fIsContainer","fIsContainer");
    $r.addProperty("IsSelected",0,rtl.boolean,"fIsSelected","fIsSelected");
    $r.addProperty("Height",3,rtl.string,"getHeight","SetHeight");
    $r.addProperty("Width",3,rtl.string,"getWidth","SetWidth");
    $r.addProperty("Top",3,rtl.longint,"getTop","SetTop");
    $r.addProperty("Left",3,rtl.longint,"getLeft","SetLeft");
    $r.addProperty("Caption",3,rtl.string,"getCaption","SetCaption");
  });
  this.CurrentPopupName = "";
  this.OpenModal = function (WindowId) {
    var Result = "";
    if ($mod.CurrentPopupName !== "") $mod.CloseModal($mod.CurrentPopupName);
    try{
       var modalwindowid= WindowId;
       //alert('open windowid='+WindowId);
        var modal = document.getElementById(modalwindowid);
       // alert('found '+modal);
        modal.style.display = 'block';
    }catch(err){alert('Error in Popup.OpenModal '+ err.message);};
    $mod.CurrentPopupName = WindowId;
    return Result;
  };
  this.CloseModal = function (WindowId) {
    var Result = "";
    var UIRootNodeName = "";
    UIRootNodeName = pas.NodeUtils.UIRootNode.NodeName;
    var modal = document.getElementById(WindowId);
    modal.style.display = "none";
    $mod.CurrentPopupName = "";
    return Result;
  };
  this.addTheModalBackground = function (ParentName, WindowId, EventType) {
    var Result = "";
    var OnClickString = "";
    if (WindowId === pas.NodeUtils.MainForm.fName) return Result;
    OnClickString = "event.target.style.display = 'none'; event.stopPropagation();";
    OnClickString = "pas.Popup.CloseModal(event.target.id); event.stopPropagation();";
    try{
      //alert('addTheModalBackground '+WindowId);
        $mod.InitialisePopupStyles()
        var HTMLString = ''
        +'<div id='+WindowId+' class="modal-background" '
        +'onclick="'+OnClickString+'">'
        +'</div>';
    
        //----- now append the declarations to the Parent -------------------------------------------
        var ParentItem=document.getElementById(ParentName);
        ParentItem.insertAdjacentHTML('beforeend', HTMLString);
    
        //alert('addTheModalBackground done');
      }catch(err) {alert('Error in Popup.addTheModalBackground '+ err.message);};
    return Result;
  };
  this.addaModalContentItem = function (MyName) {
    var Result = "";
    var ContentName = "";
    ContentName = MyName + "Contents";
    try{
      //alert('addaModalContentItem '+ContentName);
          var HTMLString = ''
          +'  <!-- Modal content -->'
          +'  <div id="'+ContentName+'" class="modal-content" > '
          +'    <div id="'+MyName+'Caption" ></div> '
          +'  </div>';
    
          var ParentItem = document.getElementById(MyName);
          ParentItem.innerHTML = ParentItem.innerHTML + HTMLString;
    
          //alert('addaModalContentItem done');
      }catch(err){alert('Error in Popup.addaModalContentItem '+ err.message);};
    return Result;
  };
  this.InitialisePopupStyles = function () {
    var Result = "";
    try{
      // ----------------------------------------check if the style has already been set
      var x = document.getElementsByTagName("STYLE");
      var StyleIsSet = false;
      if (x.length>0){
        for (var i=0; i<x.length; i++){
          var y= x[i].innerHTML;
          if (y.indexOf("modal-background") !=-1) { StyleIsSet =true}
        }
      }
      if (StyleIsSet == false){
         var ModalBackgroundStyleString = ''
         +'<style>'
          +'/* The Modal (background) */'
          +'.modal-background {'
              +'display: none; /* Hidden by default */'
              +'position: fixed; /* Stay in place */'
              +'z-index: 1; /* Sit on top */'
              +'padding-top: 10px; /* Location of the box */'
              +'left: 0;'
              +'top: 0;'
              +'width: 100%; /* Full width */'
              +'height: 100%; /* Full height */'
              //+'overflow: auto; /* Enable scroll if needed */'
              +'background-color: rgb(0,0,0); /* Fallback color */'
              +'background-color: rgba(0,0,0,0.3); /* Black w/ opacity */'
          +'} '
          +'.modal-content {'
              +'background-color: #FFFFFF;'
              +'position: absolute;'
              +'border: 1px solid #888800;'
          +'}'
          +'</style>';
        //----------------------------- now append the style declarations to the head of the HTML page
        document.head.innerHTML = document.head.innerHTML+ModalBackgroundStyleString;
      }
    }catch(err)  {alert('Error in Popup.InitialisePopupStyles '+ err.message);};
    return Result;
  };
  $mod.$init = function () {
    pas.NodeUtils.AddNodeFuncLookup("Popup",$impl.CreateinterfaceObj,$impl.CreateWidget);
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.CreateinterfaceObj = function (MyForm, Nodename) {
    var Result = null;
    var newobj = null;
    newobj = $mod.TXPopup.$create("Create$1",["UI",Nodename,"Popup",true]);
    if (MyForm !== null) {
      newobj.MyForm = MyForm}
     else {
      MyForm = pas.NodeUtils.TForm.$create("Create");
      MyForm.fName = Nodename;
      newobj.MyForm = MyForm;
    };
    newobj.myNode = newobj;
    Result = newobj;
    return Result;
  };
  $impl.CreateWidget = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    var ParentName = "";
    ParentName = ParentNode.NodeName;
    try{
        $mod.addTheModalBackground(ParentName,ScreenObjectName,"");
        $mod.addaModalContentItem(ScreenObjectName);
    
        }catch(err) { alert(err.message+' in Popup.CreateWidget');};
    MyNode.ScreenObject = MyNode;
    if (ScreenObjectName === pas.NodeUtils.MainForm.fName) {
      MyNode.SetCaption("");
      MyNode.SetHeight("100%");
      MyNode.SetWidth("100%");
      MyNode.SetTop(0);
      MyNode.SetLeft(0);
    } else {
      MyNode.SetCaption(MyNode.getCaption());
      MyNode.SetHeight(MyNode.getHeight());
      MyNode.SetWidth(MyNode.getWidth());
      MyNode.SetTop(MyNode.getTop());
      MyNode.SetLeft(MyNode.getLeft());
      MyNode.fIsContainer = true;
    };
    Result = MyNode;
    return Result;
  };
});
rtl.module("HTMLUtils",["System","Classes","SysUtils","StringUtils","NodeUtils"],function () {
  "use strict";
  var $mod = this;
  this.setPasteCutAreaSize = function () {
    var localscreentype = false;
    var SystemRootNameVar = "";
    localscreentype = false;
    SystemRootNameVar = pas.NodeUtils.SystemRootName;
    try{
      // add in the paste area in a way that does not upset android / cordova
      var rd= document.getElementById(SystemRootNameVar);
    
      rd.insertAdjacentHTML('afterend', '<textarea id = "cuttextarea"  spellcheck="false" wrap="false" ></textarea>');
    
      if (localscreentype==true){setLargeFont();};
    
      var cutTextarea = document.getElementById("cuttextarea");
      if (cutTextarea==null) {alert('cannot find element cuttextarea');}
      cutTextarea.setAttribute('tabindex', '-1'); // so it can be focused
      cutTextarea.contenteditable=true;
    
    
      } catch(err) { alert(err.message+'  in HTMLUtils.setPasteCutAreaSize'); };
  };
  this.addHandVBoxStyles = function () {
    var Result = "";
    var dummy = 0;
    dummy = 0;
    // ----------------------------------------check if the style has already been set
        var x = document.getElementsByTagName("STYLE");
        var StyleIsSet = false;
        if (x.length>0){
          for (var i=0; i<x.length; i++){
            var y= x[i].innerHTML;
            if (y.indexOf(".hbox") !=-1) { StyleIsSet =true}
          }
        }
    
    
        if (StyleIsSet == false){
            var HandVBoxStyleString = '<style>'
            +'.hbox { '
                +' margin:0px;'
                +' display: -webkit-flex;'
                +' display: -ms-flexbox;'
                +' display: -flex;'
                +' -webkit-flex-direction: row;'
                +' -ms-flex-direction: row;'
                +' flex-direction: row;'
                +' -webkit-align-content: stretch;'
                +' -ms-flex-line-pack: stretch;'
                +' align-items: stretch;'
                +' }'
              +'.hboxNoStretch { '
                  +' margin:0px;'
                  +' display: -webkit-flex;'
                  +' display: -ms-flexbox;'
                  +' display: -flex;'
                  +' -webkit-flex-direction: row;'
                  +' -ms-flex-direction: row;'
                  +' flex-direction: row;'
                  +' -webkit-align-content: flex-start;'
                  +' -ms-flex-line-pack: start;'
                  +' align-items: flex-start;'
                  +' }'
    
              +'.vbox { '
                  +' margin:0px;'
                  +' display: -webkit-flex;'
                  +' display: -ms-flexbox;'
                  +' display: flex;'
                  +' -webkit-flex-direction: column;'
                  +' -ms-flex-direction: column;'
                  +' flex-direction: column;'
                  +' -webkit-align-content: stretch;'
                  +' -ms-flex-line-pack: stretch;'
                  +' align-items: stretch;'
                  +' }'
                +'.vboxNoStretch { '
                   +' margin:0px;'
                   +' display: -webkit-flex;'
                   +' display: -ms-flexbox;'
                   +' display: flex;'
                   +' -webkit-flex-direction: column;'
                   +' -ms-flex-direction: column;'
                   +' flex-direction: column;'
                   +' -webkit-align-content: flex-start;'
                   +' -ms-flex-line-pack: start;'
                   +' align-items: flex-start;'
                   +' }'
    
    
            +'.AlignmentCentre {display: flex;'
               +'align-items: center;'
               +'justify-content: center;}'
            +'.AlignmentRight {display:flex;'
              +'align-items: flex-e'+'nd;'
              +'justify-content: flex-e'+'nd;}'
            +'.AlignmentLeft {display:flex;'
              +'align-items: flex-start;'
              +'justify-content: flex-start;}'
            +'.AlignmentTop {display:flex;'
              +'align-items: flex-start;'
              +'justify-content: flex-start;}'
            +'.AlignmentBottom {display:flex;'
              +'align-items: flex-e'+'nd;'
              +'justify-content: flex-e'+'nd;}'
    
            +'  input {'
                    +' line-height: 20px;'
                 +'}'
    
    
               +' </style>';
    
          //----------------------------- now append the style declarations to the head of the HTML page
          document.head.innerHTML = document.head.innerHTML+HandVBoxStyleString;
         };
    return Result;
  };
  this.ClearAllScreenObjects = function () {
    var SystemRootNameVar = "";
    SystemRootNameVar = pas.NodeUtils.SystemRootName;
    pas.StringUtils.ShowMessage("ClearAllScreenObjects");
    try{
       var self=document.getElementById(SystemRootNameVar);
       while (self.firstChild)
         { self.removeChild(self.firstChild);}
    }catch(err) { alert(err.message+'  in HTMLUtils.ClearAllScreenObjects');};
  };
  this.PrepareHeightWidthHTML = function (HW, StrVal, StyleVal) {
    var hw1 = "";
    if ((pas.StringUtils.FoundString(StrVal.get(),"px") > 0) || (pas.StringUtils.FoundString(StrVal.get(),"%") > 0)) {
      hw1 = StrVal.get()}
     else hw1 = StrVal.get() + "px";
    StrVal.set(hw1);
    if (HW.get() === "H") {
      StyleVal.set(("height:" + hw1) + ";");
      if (pas.StringUtils.FoundString(hw1,"px") > 0) StyleVal.set(((StyleVal.get() + "max-height:") + hw1) + ";");
    };
    if (HW.get() === "W") {
      StyleVal.set(("width:" + hw1) + ";");
      if (pas.StringUtils.FoundString(hw1,"px") > 0) StyleVal.set(((StyleVal.get() + "max-width:") + hw1) + ";");
    };
  };
  this.SetHeightWidthHTML = function (MyNode, ob, HW, AttrValue) {
    var hwStr = "";
    var StyleHW = "";
    var StyleHW0 = "";
    $mod.PrepareHeightWidthHTML({get: function () {
        return HW;
      }, set: function (v) {
        HW = v;
      }},{get: function () {
        return AttrValue;
      }, set: function (v) {
        AttrValue = v;
      }},{get: function () {
        return StyleHW0;
      }, set: function (v) {
        StyleHW0 = v;
      }});
    hwStr = AttrValue;
    StyleHW = StyleHW0;
    if (ob!=null) {
      if (HW=='H') {
        ob.style.height=hwStr; }
      else {
        ob.style.width=hwStr;  }
    };
  };
  this.DeleteScreenObject = function (MyNode) {
    var Result = "";
    var ObjName = "";
    ObjName = MyNode.NodeName;
    try{
    var ThisObject = document.getElementById(ObjName);
    if (ThisObject!=null) {
       ThisObject.parentNode.removeChild(ThisObject);
      }
    }catch(err) { alert(err.message+' in HTMLUtils.DeleteScreenObject');};
    pas.NodeUtils.NilScreenObject(MyNode);
    return Result;
  };
  this.CreateWrapperHtml = function (NewNode, ParentNode, ClassName, ScreenObjectName, ScreenObjectType) {
    var Result = "";
    var Border = "";
    var BgColor = "";
    Border = NewNode.GetAttribute("Border",true).AttribValue;
    BgColor = NewNode.GetAttribute("BgColor",true).AttribValue;
    try{
    
        // note tabindex=0 allows a div to be focused.  Only the focused element will listen to keyboard events.
    
        var Parent = pas.HTMLUtils.ScreenObjectInnerComponent(ParentNode);
    
        var ClassString =' class="';
        if (Border=='True') {ClassString = ClassString+' normal-border '+ClassName;}
        else  {ClassString = ClassString+' no-border '+ClassName;}
    
        // for vbox containers to centre their children we must mark all the children as centred
        if (Parent.classList.contains("vbox")&&(Parent.classList.contains("Centre")))
          {ClassString= ClassString +  ' hCentre '};
        ClassString= ClassString + '" ';
    
        var ComponentHTML='';
        var NodeIDString = "'"+ScreenObjectName+"'";
        var componentClick="'Click'";
        var blankParam="''";
    
        var WrapperStyle = ' background-color:'+BgColor+'; white-space:nowrap; ';
    
        var FullHTMLString='<div '+ClassString+' style="'+WrapperStyle+'" tabindex="0" position = "relative" id='+ScreenObjectName+
                    ' onclick="event.stopPropagation(); pas.Events.handleEvent('+componentClick+','+NodeIDString+', this.value,'+blankParam+');" '+
                       ' </div> ';
    
      }catch(err) { alert(err.message+'  in HTMLUtils.CreateWrapperHtml');}
    
      return FullHTMLString;
    return Result;
  };
  this.CreateWrapperDiv = function (MyNode, ParentNode, ClassName, ScreenObjectName, ScreenObjectType, position) {
    var Result = null;
    var bdr = "";
    var ShowBorder = false;
    bdr = MyNode.GetAttribute("Border",true).AttribValue;
    if (bdr !== "") {
      ShowBorder = pas.StringUtils.MyStrToBool(bdr)}
     else ShowBorder = false;
    try {
           var MyParent = pas.HTMLUtils.ScreenObjectInnerComponent(ParentNode);
           var HTMLImplementation = pas.HTMLUtils.CreateWrapperHtml(MyNode,ParentNode,'UI',ScreenObjectName,ScreenObjectType);
           pas.HTMLUtils.AddObjectToParentObject(ParentNode,MyParent.id,ScreenObjectName,position,HTMLImplementation);
           var wrapper=document.getElementById(ScreenObjectName);
           if (wrapper.style.overflow!='scroll')
           {
              wrapper.style.overflow = 'hidden';
           }
           if (ShowBorder==true) {
              wrapper.classList.add("normal-border");
           }
    
           return wrapper;
    
       } catch(err) { alert(err.message+'  in HTMLUtils.CreateWrapperDiv');};
    return Result;
  };
  this.AddObjectToParentObject = function (ParentNode, ParentId, myId, position, HTMLString) {
    var pos = 0;
    var mysib = null;
    pos = position;
    if (pos > -1) {
      if (rtl.length(ParentNode.ChildNodes) > (pos + 1)) {
        mysib = ParentNode.ChildNodes[pos + 1];
        if (mysib.ScreenObject === null) pos = -1;
      } else {
        if (pos > (rtl.length(ParentNode.ChildNodes) - 1)) pas.StringUtils.ShowMessage(((("cannot insert under " + ParentNode.NodeName) + " at position ") + pas.SysUtils.IntToStr(pos)) + ". reverted to end");
        pos = -1;
      };
    };
    try {
     var myParent=document.getElementById(ParentId);
       // Insert the new container under the given parent, at the correct sibling position
       if (pos==-1)  {
       myParent.insertAdjacentHTML('beforeend', HTMLString);
       }
       else if ( pos==0) {
       myParent.insertAdjacentHTML('afterbegin', HTMLString);
       }
       else {
         var mySibling=document.getElementById(mysib.NodeName);
         if (mySibling!=null) {
           mySibling.insertAdjacentHTML('beforebegin', HTMLString);
         }
         else {
           // insert msg here.... (1)
           var str=sibname;
           alert(str);
           myParent.insertAdjacentHTML('beforeend', HTMLString);
         }
         }
    } catch(err) { alert(err.message+'  in HTMLUtils.AddObjectToParentObject');};
  };
  this.ScreenObjectInnerComponent = function (SystemNode) {
    var Result = null;
    var innername = "";
    innername = SystemNode.NodeName + "Contents";
    Result=document.getElementById(innername);
    if (Result === null) Result=document.getElementById(SystemNode.NodeName);
    if (Result === null) pas.StringUtils.ShowMessage(("object " + SystemNode.NodeName) + " not found in HTMLUtils.ScreenObjectInnerComponent");
    return Result;
  };
  this.UnHighlight = function (ObjID, HadBorder) {
    try{
         // alert('unhighlight '+ObjID);
          var ob=document.getElementById(ObjID)
          if (ob!=null) {
            ob.classList.remove("highlight-border");
    
            if (HadBorder=='True') {
               ob.classList.add("normal-border");
               }
          }
          }catch(err) { alert(ObjID+': '+err.message+'  in HTMLUtils.UnHighlight'); };
  };
  this.Highlight = function (ObjID) {
    try{
    //alert('Highlight '+ObjID);
    var ob=document.getElementById(ObjID);
    if (ob!=null) {
      ob.classList.remove("normal-border");
      ob.classList.remove("no-border");
      ob.classList.add("highlight-border");
    }
    }catch(err) { alert(err.message+'  in HTMLUtils.Highlight'); };
  };
  this.ShowHideSelectedBorder = function (myNode, showborder) {
    var HadBorder = false;
    if (myNode.GetAttribute("Border",true).AttribValue !== "") {
      HadBorder = pas.StringUtils.MyStrToBool(myNode.GetAttribute("Border",true).AttribValue)}
     else HadBorder = false;
    //alert('set showborder to '+showborder);
    var ob = document.getElementById(this.NodeName);
    if (ob!=null) {
    if (showborder==true) {
       pas.HTMLUtils.Highlight(ob.id);
    }
    else {
       pas.HTMLUtils.UnHighlight(ob.id, HadBorder);
    }      };
  };
  this.GetDataNodeFromTreeNode = function (nodeID) {
    var Result = null;
    var bits = null;
    bits = pas.StringUtils.stringsplit(nodeID,"Contents");
    Result = pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,bits.Get(0),true);
    return Result;
  };
  this.glbBorderWidth = 3;
  this.glbLabelSpacing = 3;
  this.glbMarginSpacing = "3px";
  $mod.$init = function () {
  };
},[]);
rtl.module("Events",["System","SysUtils","Classes","NodeUtils","HTMLUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.isValidSystemData = function (SystemDescription) {
    var Result = false;
    var teststring = "";
    var teststring2 = "";
    var i = 0;
    var MatchFound = false;
    MatchFound = true;
    teststring = "xp= @|";
    teststring2 = "<Root|; Class |=R";
    for (var $l1 = 1, $end2 = teststring.length; $l1 <= $end2; $l1++) {
      i = $l1;
      if ((SystemDescription.charAt(i - 1) !== teststring.charAt(i - 1)) && (SystemDescription.charAt(i - 1) !== teststring2.charAt(i - 1))) MatchFound = false;
    };
    Result = MatchFound;
    return Result;
  };
  this.handleEvent = function (MyEventType, nodeID, myValue, PropName) {
    var CurrentNode = null;
    var DoContinue = false;
    if (MyEventType === "notnow") return;
    if (pas.NodeUtils.StartingUp === false) {
      if (((MyEventType === "TreeNodeClick") || (MyEventType === "DragStart")) || (MyEventType === "Drop")) {
        CurrentNode = pas.HTMLUtils.GetDataNodeFromTreeNode(nodeID);
      } else CurrentNode = pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,nodeID,false);
      if ((CurrentNode !== null) && (pas.NodeUtils.MainForm !== null)) {
        DoContinue = true;
        if ((PropName !== "") && (PropName !== "undefined")) {
          pas.NodeUtils.PushTolinks(CurrentNode,PropName,myValue,pas.NodeUtils.SystemNodeTree);
        };
        var fn=null;
        if (pas.NodeUtils.MainForm!=null) {
          fn = pas.NodeUtils.MainForm['HandleGenericEvent'];
          if (fn!=null)  {
             //alert('running generic handler');
             var cn = fn(MyEventType,myValue,CurrentNode);
             DoContinue=cn;
             }
        };
        if (DoContinue) {
          $impl.ExecuteEventHandlers(MyEventType,CurrentNode.NodeName,myValue,CurrentNode);
        };
      };
    };
  };
  this.handleEvent$1 = function (MyEventType, nodeID, myValue) {
    $mod.handleEvent(MyEventType,nodeID,myValue,"");
  };
  this.SuppressEvents = false;
  $mod.$init = function () {
    $mod.handleEvent$1("notnow","notnow","");
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.RunComponentEvent = function (myName, EventType, MyNode, MyValue) {
    try {
    //alert('RunComponentEvent looking for '+myName+'Handle'+EventType);
    //alert('RunComponentEvent NodeName='+MyNode.NodeName);
    
      var fn=null;
      if (MyNode.MyForm!=null) {
        fn = MyNode.MyForm[myName+'Handle'+EventType];
        if (fn!=null) {
          fn = fn.bind(MyNode.MyForm);     // so that the 'this' context will be preserved
        }
      }
    
      if (fn==null) {
      // the component may have been created dynamically at run-time.
      // in which case look for a registered event.
        fn = MyNode.FindRegisteredEvent(EventType);
      }
      if (fn!=null)  {
         fn(myName,MyNode,MyValue);
         // alert('function done.');
      }
    }catch(err) { alert(err.message+'  in NodeUtils.RunComponentEvent '+myName+' '+EventType);};
  };
  $impl.ExecuteEventHandlers = function (MyEventType, nodeID, myValue, myNode) {
    var Result = "";
    var i = 0;
    var NumHandlers = 0;
    NumHandlers = myNode.myEventTypes.GetCount();
    for (var $l1 = 0, $end2 = NumHandlers - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      if (myNode.myEventTypes.Get(i) === MyEventType) {
        $impl.RunComponentEvent(nodeID,MyEventType,myNode,myValue);
      };
    };
    return Result;
  };
});
rtl.module("WrapperPanel",["System","Classes","SysUtils","StringUtils","NodeUtils","HTMLUtils","Events"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"TWrapperPanel",pas.NodeUtils.TInterfaceObject,function () {
    this.$init = function () {
      pas.NodeUtils.TInterfaceObject.$init.call(this);
      this.FIsContainer = false;
      this.FIsSelected = false;
      this.FAlignChildrenVertical = false;
    };
    this.GetName = function () {
      var Result = "";
      Result = this.NodeName;
      return Result;
    };
    this.GetIsVisible = function () {
      var Result = false;
      var tmp = "";
      if (this.myNode !== null) {
        tmp = this.myNode.GetAttribute("IsVisible",true).AttribValue;
        if (tmp === "") tmp = "True";
        Result = pas.StringUtils.MyStrToBool(tmp);
      } else Result = true;
      return Result;
    };
    this.GetHint = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("Hint",true).AttribValue;
      return Result;
    };
    this.GetmyWidth = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("myWidth",true).AttribValue;
      return Result;
    };
    this.GetmyHeight = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("myHeight",true).AttribValue;
      return Result;
    };
    this.GetBgColor = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("BgColor",true).AttribValue;
      return Result;
    };
    this.GetLabelText = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("LabelText",true).AttribValue;
      return Result;
    };
    this.GetLabelPos = function () {
      var Result = "";
      if (this.myNode !== null) {
        Result = this.myNode.GetAttribute("LabelPos",true).AttribValue}
       else Result = "Top";
      return Result;
    };
    this.GetAlignment = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("Alignment",true).AttribValue;
      return Result;
    };
    this.SetMyName = function (AValue) {
      this.SetMyName(AValue);
      var ob = document.getElementById(this.NodeName);
      inner = pas.HTMLUtils.ScreenObjectInnerComponent(this);
      if (inner.id == this.NodeName+'Contents') {
        inner.id = AValue+'Contents';
        }
         //!!!! issue here with naming of html components / references within event handlers / inner components / etc
      ob.id = AValue;
      if (this.myNode !== null) this.myNode.NodeName = AValue;
    };
    this.SetIsVisible = function (AValue) {
      if (this.myNode !== null) this.myNode.SetAttributeValue$1("IsVisible",pas.StringUtils.MyBoolToStr(AValue));
      //alert('set visible '+AValue+' for '+this.NodeName);
      var ob = document.getElementById(this.NodeName);
      if (ob!=null)  {
        if (AValue==true) {
          ob.style.display = 'block';
        }
        else  {
          ob.style.display = 'none';
        }
      };
    };
    this.SetIsSelected = function (AValue) {
      if (AValue !== this.FIsSelected) {
        this.FIsSelected = AValue;
        pas.HTMLUtils.ShowHideSelectedBorder(this,this.FIsSelected);
      };
    };
    this.SetHint = function (AValue) {
      this.myNode.SetAttributeValue$1("Hint",AValue);
      var ob = document.getElementById(this.NodeName);
      if (ob!=null)  {
      ob.title=AValue; };
    };
    this.SetLabelText = function (AValue) {
      var tmp = "";
      this.myNode.SetAttributeValue$1("LabelText",AValue);
      tmp = this.myNode.NodeName;
      var ob = document.getElementById(this.NodeName+'ContentsLbl');
      if (ob!=null) {
         ob.innerHTML=AValue;   };
    };
    this.SetLabelPos = function (AValue) {
      this.myNode.SetAttributeValue$1("LabelPos",AValue);
      this.SortOutAlignmentAndLabelPos();
    };
    this.SetAlignment = function (AValue) {
      if (this.myNode !== null) {
        this.myNode.SetAttributeValue$1("Alignment",AValue);
        this.SortOutAlignmentAndLabelPos();
      };
    };
    this.SortOutAlignmentAndLabelPos = function () {
      var ParentAlignChildrenVertical = false;
      var MyAlignment = "";
      var MyLabelPos = "";
      var ParentNode = null;
      MyAlignment = this.GetAlignment();
      MyLabelPos = this.GetLabelPos();
      ParentNode = pas.NodeUtils.FindParentOfNode$1(pas.NodeUtils.SystemNodeTree,this.NodeName);
      ParentAlignChildrenVertical = ParentNode.FAlignChildrenVertical;
      if ((MyAlignment === "Right") || (MyAlignment === "Left")) {
        if (ParentAlignChildrenVertical === false) MyAlignment = "Top";
      } else if ((MyAlignment === "Top") || (MyAlignment === "Bottom")) {
        if (ParentAlignChildrenVertical === true) MyAlignment = "Left";
      };
      try {
             var ob = document.getElementById(this.NodeName+'Contents');
             var lbl = document.getElementById(this.NodeName+'ContentsLbl');
             var wrapper = document.getElementById(this.NodeName);
             var lp = MyLabelPos;
      
             if ((ob!=null) && (wrapper!=null)) {
             wrapper.classList.remove('hbox');
             wrapper.classList.remove('hboxNoStretch');
             wrapper.classList.remove('vbox');
             wrapper.classList.remove('vboxNoStretch');
             wrapper.classList.remove('AlignmentCentre');
             wrapper.classList.remove('AlignmentRight');
             wrapper.classList.remove('AlignmentLeft');
             wrapper.classList.remove('AlignmentTop');
             wrapper.classList.remove('AlignmentBottom');
      
             if (lbl!=null) {
               lbl.style.padding='0px';
      
               if (lp=='Left') {
                 lbl.parentNode.insertBefore(lbl, ob);  //put lbl before ob
                 wrapper.classList.add('hboxNoStretch');
                 //lbl.style.verticalAlign='left';
                 lbl.style.alignSelf='center';
                 lbl.style.padding='0px 3px 0px 0px';               // t,r,b,l
               }
               else if (lp=='Right') {
                 ob.parentNode.insertBefore(ob, lbl);  //put lbl after ob
                 wrapper.classList.add('hboxNoStretch');
                 //lbl.style.verticalAlign='right';
                 lbl.style.alignSelf='center';
                 lbl.style.padding='0px 0px 0px 3px';               // t,r,b,l
               }
               else if (lp=='Top') {
                 ob.parentNode.insertBefore(lbl, ob);
                 wrapper.classList.add('vboxNoStretch');
                 //lbl.style.verticalAlign='top';
                 lbl.style.alignSelf='center';
                 lbl.style.padding='0px 0px 0px 3px';               // t,r,b,l
               }
               else if (lp=='Bottom') {
                 ob.parentNode.insertBefore(ob, lbl);
                 wrapper.classList.add('vboxNoStretch');
                 //lbl.style.verticalAlign='bottom';
                 lbl.style.alignSelf='center';
                 lbl.style.padding='3px 0px 0px 0px';               // t,r,b,l
               }
             }
      
      
             if (MyAlignment=='Right') {
               if (ParentAlignChildrenVertical) {
               ob.style.float='right';
               wrapper.classList.add('AlignmentRight');
               if (lbl!=null) {
                 lbl.style.float='right';
                 if ((lp=='Top')||(lp=='Bottom')) {
                     lbl.style.alignSelf='flex-e'+'nd';
                 }
               }
             }
             }
             else if (MyAlignment=='Left') {
             if (ParentAlignChildrenVertical) {
                 ob.style.float='left';
                 wrapper.classList.add('AlignmentLeft');
                 if (lbl!=null) {
                   lbl.style.float='left';
                   if ((lp=='Top')||(lp=='Bottom')) {
                       lbl.style.alignSelf='flex-start';
                   }
                 }
               }
               }
             else if (MyAlignment=='Centre') {
               ob.style.float='left';
                wrapper.classList.add('AlignmentCentre');
                if (lbl!=null) {
                   lbl.style.float='left';
                }
             }
      
             else if (MyAlignment=='Top') {
             if (ParentAlignChildrenVertical==false) {
               ob.style.float='left';
               wrapper.classList.add('AlignmentTop');
               if (lbl!=null) {
                 lbl.style.float='left';
                 if ((lp=='Left')||(lp=='Right')) {
                     lbl.style.alignSelf='flex-start';
                   }
               }
              }
              }
             else if (MyAlignment=='Bottom') {
             if (ParentAlignChildrenVertical==false) {
               ob.style.float='left';
               wrapper.classList.add('AlignmentBottom');
               if (lbl!=null) {
                lbl.style.float='left';
                if ((lp=='Left')||(lp=='Right')) {
                     lbl.style.alignSelf='flex-e'+'nd';
                     }
               }
           }
          }
      
      
         }
       } catch(err) { alert(err.message+'  in WrapperPanel.SortOutAlignmentAndLabelPos'); };
    };
    this.Create$2 = function (NodeName) {
      pas.NodeUtils.TDataNode.Create$1.call(this,"UI",NodeName,"",false);
      this.FAlignChildrenVertical = true;
      this.NodeName = NodeName;
      this.FIsContainer = true;
      this.SetIsVisible(true);
      this.myNode = this;
      pas.NodeUtils.AddChildToParentNode({p: pas.NodeUtils, get: function () {
          return this.p.SystemNodeTree;
        }, set: function (v) {
          this.p.SystemNodeTree = v;
        }},{p: this, get: function () {
          return this.p.myNode;
        }, set: function (v) {
          this.p.myNode = v;
        }},-1);
      this.SetHint("");
      this.SetBgColor("#FFFFFF");
    };
    this.SetBgColor = function (AValue) {
      this.SetAttributeValue("BgColor",AValue,"Color");
      try {
      var ob = document.getElementById(this.NodeName);
      if (ob!=null) {
      ob.style.backgroundColor = AValue;  }
      } catch(err) { alert(err.message+'  in WrapperPanel.SetBgColor'); };
    };
    this.SetmyHeight = function (AValue) {
      this.myNode.SetAttributeValue$1("myHeight",AValue);
      var ob = document.getElementById(this.NodeName);
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
    };
    this.SetmyWidth = function (AValue) {
      this.myNode.SetAttributeValue$1("myWidth",AValue);
      var ob = document.getElementById(this.NodeName);
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
    };
    var $r = this.$rtti;
    $r.addProperty("IsContainer",0,rtl.boolean,"FIsContainer","FIsContainer");
    $r.addProperty("IsSelected",2,rtl.boolean,"FIsSelected","SetIsSelected",{Default: false});
    $r.addProperty("IsVisible",3,rtl.boolean,"GetIsVisible","SetIsVisible");
    $r.addProperty("Hint",3,rtl.string,"GetHint","SetHint");
    $r.addProperty("Name",3,rtl.string,"GetName","SetMyName");
    $r.addProperty("myWidth",3,rtl.string,"GetmyWidth","SetmyWidth");
    $r.addProperty("myHeight",3,rtl.string,"GetmyHeight","SetmyHeight");
    $r.addProperty("BgColor",3,rtl.string,"GetBgColor","SetBgColor");
    $r.addProperty("LabelPos",3,rtl.string,"GetLabelPos","SetLabelPos");
    $r.addProperty("Alignment",3,rtl.string,"GetAlignment","SetAlignment");
    $r.addProperty("LabelText",3,rtl.string,"GetLabelText","SetLabelText");
    $r.addProperty("AlignChildrenVertical",0,rtl.boolean,"FAlignChildrenVertical","FAlignChildrenVertical");
  });
  this.SuppressDesignerProperty = function (Classname, pName) {
    if ($mod.FindSuppressedProperty(Classname,pName) < 0) {
      $mod.SuppressedDesignerProperties = rtl.arraySetLength($mod.SuppressedDesignerProperties,$mod.TSuppressedDesignerProperty,rtl.length($mod.SuppressedDesignerProperties) + 1);
      $mod.SuppressedDesignerProperties[rtl.length($mod.SuppressedDesignerProperties) - 1].ClassName = Classname;
      $mod.SuppressedDesignerProperties[rtl.length($mod.SuppressedDesignerProperties) - 1].PName = pName;
    };
  };
  this.TSuppressedDesignerProperty = function (s) {
    if (s) {
      this.ClassName = s.ClassName;
      this.PName = s.PName;
    } else {
      this.ClassName = "";
      this.PName = "";
    };
    this.$equal = function (b) {
      return (this.ClassName === b.ClassName) && (this.PName === b.PName);
    };
  };
  this.SuppressedDesignerProperties = [];
  this.FindSuppressedProperty = function (Classname, pName) {
    var Result = 0;
    var i = 0;
    Result = -1;
    i = 0;
    while (i < rtl.length($mod.SuppressedDesignerProperties)) {
      if (($mod.SuppressedDesignerProperties[i].ClassName === Classname) && ($mod.SuppressedDesignerProperties[i].PName === pName)) {
        Result = i;
        i = rtl.length($mod.SuppressedDesignerProperties);
      };
      i = i + 1;
    };
    return Result;
  };
  $mod.$init = function () {
    $mod.SuppressedDesignerProperties = rtl.arraySetLength($mod.SuppressedDesignerProperties,$mod.TSuppressedDesignerProperty,0);
  };
});
rtl.module("NodeUtils",["System","Classes","SysUtils","TypInfo","StringUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $mod.$rtti.$Class("TDataNode");
  rtl.createClass($mod,"TForm",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fName = "";
    };
    var $r = this.$rtti;
    $r.addProperty("Name",0,rtl.string,"fName","fName");
  });
  rtl.createClass($mod,"TXPropertyLink",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FTIObjectName = "";
      this.FTIObject = null;
      this.FTIPropertyName = "";
    };
    this.$final = function () {
      this.FTIObject = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    var $r = this.$rtti;
    $r.addProperty("TIObjectName",0,rtl.string,"FTIObjectName","FTIObjectName");
    $r.addProperty("TIObject",0,pas.System.$rtti["TObject"],"FTIObject","FTIObject");
    $r.addProperty("TIPropertyName",0,rtl.string,"FTIPropertyName","FTIPropertyName");
  });
  this.TNodeAttribute = function (s) {
    if (s) {
      this.AttribName = s.AttribName;
      this.AttribType = s.AttribType;
      this.AttribValue = s.AttribValue;
      this.AttribReadOnly = s.AttribReadOnly;
    } else {
      this.AttribName = "";
      this.AttribType = "";
      this.AttribValue = "";
      this.AttribReadOnly = false;
    };
    this.$equal = function (b) {
      return (this.AttribName === b.AttribName) && ((this.AttribType === b.AttribType) && ((this.AttribValue === b.AttribValue) && (this.AttribReadOnly === b.AttribReadOnly)));
    };
  };
  this.TNodeFuncsLookup = function (s) {
    if (s) {
      this.NodeType = s.NodeType;
      this.ScreenObjFunctionPtr = s.ScreenObjFunctionPtr;
      this.InObFunctionPtr = s.InObFunctionPtr;
    } else {
      this.NodeType = "";
      this.ScreenObjFunctionPtr = null;
      this.InObFunctionPtr = null;
    };
    this.$equal = function (b) {
      return (this.NodeType === b.NodeType) && (rtl.eqCallback(this.ScreenObjFunctionPtr,b.ScreenObjFunctionPtr) && rtl.eqCallback(this.InObFunctionPtr,b.InObFunctionPtr));
    };
  };
  rtl.createClass($mod,"TComponentTag",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.HH = "";
      this.WW = "";
    };
  });
  rtl.createClass($mod,"TNodeTimerTag",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.myNode = null;
      this.DestinationNode = null;
      this.pos = 0;
    };
    this.$final = function () {
      this.myNode = undefined;
      this.DestinationNode = undefined;
      pas.System.TObject.$final.call(this);
    };
  });
  rtl.createClass($mod,"TDataNode",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.NodeName = "";
      this.NodeType = "";
      this.NodeClass = "";
      this.IsDynamic = false;
      this.ScreenObject = null;
      this.MyForm = null;
      this.NodeAttributes = [];
      this.ChildNodes = [];
      this.myEventTypes = null;
      this.myEventHandlers = [];
    };
    this.$final = function () {
      this.ScreenObject = undefined;
      this.MyForm = undefined;
      this.NodeAttributes = undefined;
      this.ChildNodes = undefined;
      this.myEventTypes = undefined;
      this.myEventHandlers = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (MyClass, MyName, MyType, NodeIsDynamic) {
      this.ChildNodes = rtl.arraySetLength(this.ChildNodes,null,0);
      this.NodeAttributes = rtl.arraySetLength(this.NodeAttributes,$mod.TNodeAttribute,0);
      this.myEventTypes = pas.Classes.TStringList.$create("Create$1");
      this.myEventHandlers = rtl.arraySetLength(this.myEventHandlers,null,0);
      this.NodeClass = MyClass;
      this.NodeName = MyName;
      this.NodeType = MyType;
      this.IsDynamic = NodeIsDynamic;
    };
    this.DeleteMe = function () {
      this.ScreenObject = null;
      this.$destroy("Destroy");
    };
    this.GetAttribute = function (AttrName, AllowSpace) {
      var Result = new $mod.TNodeAttribute();
      var i = 0;
      var foundAttrib = new $mod.TNodeAttribute();
      var myAttribs = [];
      i = 0;
      foundAttrib.AttribName = "";
      myAttribs = this.NodeAttributes;
      while (i < rtl.length(myAttribs)) {
        if (this.NodeAttributes[i].AttribName === AttrName) {
          foundAttrib = new $mod.TNodeAttribute(this.NodeAttributes[i]);
          i = rtl.length(myAttribs);
        };
        i = i + 1;
      };
      if (foundAttrib.AttribName !== AttrName) if (AllowSpace) {
        foundAttrib.AttribName = AttrName;
        foundAttrib.AttribType = "";
        foundAttrib.AttribValue = "";
        foundAttrib.AttribReadOnly = false;
      } else pas.StringUtils.ShowMessage((("Attribute " + AttrName) + " not found in node ") + this.NodeName);
      Result = new $mod.TNodeAttribute(foundAttrib);
      return Result;
    };
    this.AddAttribute = function (AttributeName, AttributeType, AttributeValue, AttributeReadOnly) {
      var numAttributes = 0;
      var myAttributes = [];
      myAttributes = this.NodeAttributes;
      numAttributes = rtl.length(myAttributes);
      this.NodeAttributes = rtl.arraySetLength(this.NodeAttributes,$mod.TNodeAttribute,numAttributes + 1);
      this.NodeAttributes[numAttributes].AttribName = AttributeName;
      this.NodeAttributes[numAttributes].AttribValue = AttributeValue;
      this.NodeAttributes[numAttributes].AttribType = AttributeType;
      this.NodeAttributes[numAttributes].AttribReadOnly = AttributeReadOnly;
    };
    this.SetAttributeValue = function (AttrName, NewValue, AttrType) {
      var foundAttrib = new $mod.TNodeAttribute();
      var myAttribs = [];
      var i = 0;
      var found = false;
      foundAttrib = new $mod.TNodeAttribute(this.GetAttribute(AttrName,true));
      if (foundAttrib.AttribName === "") {
        foundAttrib.AttribName = AttrName;
        if (AttrType === "") {
          foundAttrib.AttribType = "String"}
         else foundAttrib.AttribType = AttrType;
        foundAttrib.AttribReadOnly = false;
      };
      if (foundAttrib.AttribName === AttrName) {
        foundAttrib.AttribValue = NewValue;
        if ((foundAttrib.AttribType === "") && (AttrType !== "")) foundAttrib.AttribType = AttrType;
      };
      found = false;
      myAttribs = this.NodeAttributes;
      i = 0;
      while (i < rtl.length(myAttribs)) {
        if (this.NodeAttributes[i].AttribName === AttrName) {
          this.NodeAttributes[i] = new $mod.TNodeAttribute(foundAttrib);
          i = rtl.length(myAttribs);
          found = true;
        };
        i = i + 1;
      };
      if (!found) this.AddAttribute(foundAttrib.AttribName,foundAttrib.AttribType,foundAttrib.AttribValue,foundAttrib.AttribReadOnly);
    };
    this.SetAttributeValue$1 = function (AttrName, NewValue) {
      this.SetAttributeValue(AttrName,NewValue,"");
    };
    this.GetChildIndex = function (ChildNode) {
      var Result = 0;
      var i = 0;
      var mychildren = [];
      Result = -1;
      mychildren = this.ChildNodes;
      for (var $l1 = 0, $end2 = rtl.length(mychildren) - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        if (mychildren[i].NodeName === ChildNode.NodeName) Result = i;
      };
      return Result;
    };
    this.RemoveChildNode = function (ChildNode) {
      var i = 0;
      var l = 0;
      var found = false;
      var mychildren = [];
      found = false;
      mychildren = this.ChildNodes;
      l = rtl.length(mychildren);
      i = 0;
      while (i < rtl.length(mychildren)) {
        if (found === false) {
          if (mychildren[i] === ChildNode) {
            found = true;
          };
        };
        if (found && (i < rtl.length(mychildren))) mychildren[i] = mychildren[i + 1];
        i = i + 1;
      };
      if (found) mychildren = rtl.arraySetLength(mychildren,null,l - 1);
      this.ChildNodes = mychildren;
    };
    this.FindRegisteredEvent = function (EventType) {
      var Result = null;
      var i = 0;
      Result = null;
      for (var $l1 = 0, $end2 = this.myEventTypes.GetCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        if (this.myEventTypes.Get(i) === EventType) Result = this.myEventHandlers[i];
      };
      return Result;
    };
    this.RegisterEvent = function (EventType, TheHandler) {
      var i = 0;
      for (var $l1 = 0, $end2 = this.myEventTypes.GetCount() - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        if (this.myEventTypes.Get(i) === EventType) {
          this.myEventHandlers[i] = TheHandler;
        };
      };
    };
  });
  rtl.createClass($mod,"TInterfaceObject",$mod.TDataNode,function () {
    this.$init = function () {
      $mod.TDataNode.$init.call(this);
      this.FLink = null;
      this.myNode = null;
    };
    this.$final = function () {
      this.FLink = undefined;
      this.myNode = undefined;
      $mod.TDataNode.$final.call(this);
    };
    this.SetLink = function (AValue) {
      if (this.FLink === AValue) return;
      this.FLink = AValue;
    };
    this.LinkLoadFromProperty = function (Sender) {
      if (Sender === null) ;
      this.SetAttributeValue$1("Link",$impl.LinkToStr(this.FLink));
    };
    this.LinkSaveToProperty = function (Sender) {
    };
    var $r = this.$rtti;
    $r.addField("myNode",$mod.$rtti["TDataNode"]);
    $r.addMethod("SetLink",0,[["AValue",$mod.$rtti["TXPropertyLink"],2]]);
    $r.addMethod("LinkLoadFromProperty",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("LinkSaveToProperty",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addProperty("Link",2,$mod.$rtti["TXPropertyLink"],"FLink","SetLink");
  });
  this.AddFormToNodeTree = function (myForm) {
    var Result = null;
    var myNode = null;
    myNode = null;
    myNode = pas.Popup.TXPopup.$create("Create$1",["UI",myForm.fName,"Popup",false]);
    myNode.ScreenObject = myForm;
    myNode.MyForm = myForm;
    $mod.AddChildToParentNode({p: $mod, get: function () {
        return this.p.UIRootNode;
      }, set: function (v) {
        this.p.UIRootNode = v;
      }},{get: function () {
        return myNode;
      }, set: function (v) {
        myNode = v;
      }},-1);
    Result = myNode;
    return Result;
  };
  this.AddAttrib = function (AttrParams, attrName, attrType, attrValue, attrReadOnly) {
    var i = 0;
    i = rtl.length(AttrParams.get());
    AttrParams.set(rtl.arraySetLength(AttrParams.get(),$mod.TNodeAttribute,i + 1));
    AttrParams.get()[i] = new $mod.TNodeAttribute($impl.MakeAttrib(attrName,attrType,attrValue,attrReadOnly));
  };
  this.AddChildToParentNode = function (ParentNode, ChildNode, position) {
    var numchildren = 0;
    var i = 0;
    var pn = null;
    pn = $mod.FindParentOfNode($mod.SystemNodeTree,ChildNode.get().NodeName,false);
    if (pn !== null) {
      pn.RemoveChildNode(ChildNode.get());
    };
    numchildren = rtl.length(ParentNode.get().ChildNodes);
    ParentNode.get().ChildNodes = rtl.arraySetLength(ParentNode.get().ChildNodes,null,numchildren + 1);
    if (position === -1) {
      ParentNode.get().ChildNodes[numchildren] = ChildNode.get();
    } else {
      for (var $l1 = numchildren, $end2 = position + 1; $l1 >= $end2; $l1--) {
        i = $l1;
        ParentNode.get().ChildNodes[i] = ParentNode.get().ChildNodes[i - 1];
      };
      ParentNode.get().ChildNodes[position] = ChildNode.get();
    };
  };
  this.NodeTreeToXML = function (CurrentItem, ParentNode, IncludeDynamic) {
    var Result = "";
    var XMLString = "";
    var ParentName = "";
    var i = 0;
    var numchildren = 0;
    var numAttributes = 0;
    var CurrentChildNodes = [];
    var myAttribs = [];
    XMLString = "";
    if (ParentNode !== null) ParentName = ParentNode.NodeName;
    if ((((CurrentItem.NodeClass === "Root") || (CurrentItem.NodeClass === "UI")) || (CurrentItem.NodeClass === "SVG")) || (CurrentItem.NodeClass === "Code")) {
      XMLString = ($mod.StartXMLString + CurrentItem.NodeType) + $mod.attributeListdelimiter;
      XMLString = (((XMLString + " Class ") + $mod.NameValuePairdelimiter) + CurrentItem.NodeClass) + $mod.attributeListdelimiter;
      XMLString = (((XMLString + " Name ") + $mod.NameValuePairdelimiter) + CurrentItem.NodeName) + $mod.attributeListdelimiter;
      myAttribs = CurrentItem.NodeAttributes;
      numAttributes = rtl.length(myAttribs);
      for (var $l1 = 0, $end2 = numAttributes - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        if ((CurrentItem.NodeAttributes[i].AttribName !== "ParentName") && ((CurrentItem.NodeAttributes[i].AttribName !== "XMLString") || (CurrentItem.IsDynamic === false))) XMLString = ((((((((XMLString + CurrentItem.NodeAttributes[i].AttribName) + $mod.AttribBitsDelimiter) + " ") + CurrentItem.NodeAttributes[i].AttribType) + $mod.AttribBitsDelimiter) + $impl.SubstituteSpecials(CurrentItem.NodeAttributes[i].AttribValue)) + $mod.AttribBitsDelimiter) + pas.StringUtils.MyBoolToStr(CurrentItem.NodeAttributes[i].AttribReadOnly)) + $mod.attributeListdelimiter;
      };
      XMLString = (((((((XMLString + "ParentName") + $mod.AttribBitsDelimiter) + " String") + $mod.AttribBitsDelimiter) + $impl.SubstituteSpecials(ParentName)) + $mod.AttribBitsDelimiter) + "True") + $mod.attributeListdelimiter;
      XMLString = XMLString + $mod.EndXMLString;
      CurrentChildNodes = CurrentItem.ChildNodes;
      numchildren = rtl.length(CurrentChildNodes);
      for (var $l3 = 0, $end4 = numchildren - 1; $l3 <= $end4; $l3++) {
        i = $l3;
        XMLString = XMLString + $mod.NodeTreeToXML(CurrentItem.ChildNodes[i],CurrentItem,IncludeDynamic);
      };
      XMLString = (((XMLString + $mod.StartXMLString) + $mod.ToggleFlagMarker) + CurrentItem.NodeType) + $mod.EndXMLString;
    };
    Result = XMLString;
    return Result;
  };
  this.SaveSystem = function (ToClip) {
    var systemstring = "";
    var fullstring = "";
    systemstring = $mod.NodeTreeToXML($mod.SystemNodeTree,null,true);
    fullstring = systemstring;
    if (ToClip) ;
  };
  this.FindDataNodeById = function (InTree, ScreenObjectID, showerror) {
    var Result = null;
    var FoundItem = null;
    var TempItem = null;
    var FoundParent = null;
    FoundItem = null;
    FoundParent = null;
    TempItem = $impl.ScanChildrenForNode(InTree,ScreenObjectID,{get: function () {
        return FoundParent;
      }, set: function (v) {
        FoundParent = v;
      }});
    if (TempItem !== null) {
      FoundItem = TempItem;
    } else if (showerror) pas.StringUtils.ShowMessage(("Error in NodeUtils.FindDataNodeById >" + ScreenObjectID) + "< not found");
    Result = FoundItem;
    return Result;
  };
  this.FindParentOfNode = function (InTree, ScreenObjectID, showError) {
    var Result = null;
    var FoundItem = null;
    var TempItem = null;
    var FoundParent = null;
    FoundItem = null;
    TempItem = null;
    FoundParent = null;
    TempItem = $impl.ScanChildrenForNode(InTree,ScreenObjectID,{get: function () {
        return FoundParent;
      }, set: function (v) {
        FoundParent = v;
      }});
    if ((TempItem !== null) && (FoundParent !== null)) {
      FoundItem = FoundParent;
    } else if (showError) pas.StringUtils.ShowMessage(("Error in Nodeutils.FindParentOfNode >" + ScreenObjectID) + "< not found");
    Result = FoundItem;
    return Result;
  };
  this.FindParentOfNode$1 = function (InTree, ScreenObjectID) {
    var Result = null;
    Result = $mod.FindParentOfNode(InTree,ScreenObjectID,true);
    return Result;
  };
  this.ReParentNode = function (MyNode, NewParent) {
    var OldParent = null;
    OldParent = $mod.FindParentOfNode($mod.SystemNodeTree,MyNode.NodeName,false);
    if (OldParent !== null) {
      OldParent.RemoveChildNode(MyNode);
    };
    $mod.AddChildToParentNode({get: function () {
        return NewParent;
      }, set: function (v) {
        NewParent = v;
      }},{get: function () {
        return MyNode;
      }, set: function (v) {
        MyNode = v;
      }},-1);
  };
  this.DeleteNode = function (ParentNode, MyNode) {
    var siblings = [];
    $mod.SuppressEvents = true;
    $mod.DeleteNodeChildren(MyNode);
    $impl.DeleteScreenObject(MyNode);
    siblings = rtl.arraySetLength(siblings,null,0);
    if (ParentNode === null) ParentNode = $mod.FindParentOfNode$1($mod.SystemNodeTree,MyNode.NodeName);
    if (ParentNode !== null) {
      ParentNode.RemoveChildNode(MyNode);
    };
    MyNode.DeleteMe();
    $mod.SuppressEvents = false;
  };
  this.CopyNode = function (SourceNode) {
    var Result = null;
    var NewNode = null;
    var myAttribs = [];
    var i = 0;
    myAttribs = rtl.arraySetLength(myAttribs,$mod.TNodeAttribute,rtl.length(SourceNode.NodeAttributes));
    for (var $l1 = 0, $end2 = rtl.length(SourceNode.NodeAttributes) - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      myAttribs[i] = new $mod.TNodeAttribute(SourceNode.NodeAttributes[i]);
    };
    NewNode = $mod.TDataNode.$create("Create$1",[SourceNode.NodeClass,SourceNode.NodeName,SourceNode.NodeType,false]);
    NewNode.IsDynamic = true;
    NewNode.NodeAttributes = myAttribs;
    NewNode.myEventTypes = pas.Classes.TStringList.$create("Create$1");
    NewNode.myEventHandlers = rtl.arraySetLength(NewNode.myEventHandlers,null,SourceNode.myEventTypes.GetCount());
    for (var $l3 = 0, $end4 = SourceNode.myEventTypes.GetCount() - 1; $l3 <= $end4; $l3++) {
      i = $l3;
      NewNode.myEventTypes.Add(SourceNode.myEventTypes.Get(i));
      NewNode.myEventHandlers[i] = null;
    };
    NewNode.ChildNodes = rtl.arraySetLength(NewNode.ChildNodes,null,rtl.length(SourceNode.ChildNodes));
    for (var $l5 = 0, $end6 = rtl.length(SourceNode.ChildNodes) - 1; $l5 <= $end6; $l5++) {
      i = $l5;
      NewNode.ChildNodes[i] = $mod.CopyNode(SourceNode.ChildNodes[i]);
    };
    Result = NewNode;
    return Result;
  };
  this.AddChildToDataNode = function (ParentNode, MyClass, MyName, MyType, MyAttributes, position) {
    var Result = null;
    var numchildren = 0;
    var tempDataNodeArray = [];
    var newNode = null;
    var i = 0;
    if ($mod.NodeNameIsUnique(MyName,true)) {
      tempDataNodeArray = ParentNode.ChildNodes;
      numchildren = rtl.length(tempDataNodeArray);
      ParentNode.ChildNodes = rtl.arraySetLength(ParentNode.ChildNodes,null,numchildren + 1);
      newNode = $mod.TDataNode.$create("Create$1",[MyClass,MyName,MyType,false]);
      newNode.NodeAttributes = MyAttributes;
      if (position === -1) {
        ParentNode.ChildNodes[numchildren] = newNode;
        Result = ParentNode.ChildNodes[numchildren];
      } else {
        for (var $l1 = numchildren, $end2 = position + 1; $l1 >= $end2; $l1--) {
          i = $l1;
          ParentNode.ChildNodes[i] = ParentNode.ChildNodes[i - 1];
        };
        ParentNode.ChildNodes[position] = newNode;
        Result = ParentNode.ChildNodes[position];
      };
    } else Result = null;
    return Result;
  };
  this.LookupComponentFunc = function (NodeType) {
    var Result = null;
    var i = 0;
    i = 0;
    while (i < rtl.length($mod.NodeFuncsLookup)) {
      if ($mod.NodeFuncsLookup[i].NodeType === NodeType) {
        Result = $mod.NodeFuncsLookup[i].ScreenObjFunctionPtr;
        i = rtl.length($mod.NodeFuncsLookup);
      };
      i = i + 1;
    };
    return Result;
  };
  this.NodeIsDescendantOf = function (ThisNode, AncestorName) {
    var Result = 0;
    var myresult = 0;
    var parentNode = null;
    var CurrentNode = null;
    var done = false;
    myresult = -1;
    if (ThisNode.NodeName === AncestorName) {
      myresult = 0}
     else {
      done = false;
      CurrentNode = ThisNode;
      while (done === false) {
        if (CurrentNode.NodeName === $mod.SystemRootName) {
          done = true;
          myresult = -1;
        } else {
          parentNode = $mod.FindParentOfNode$1($mod.SystemNodeTree,CurrentNode.NodeName);
          if (parentNode !== null) {
            myresult = myresult + 1;
            if (parentNode.NodeName === AncestorName) {
              done = true;
            } else CurrentNode = parentNode;
          } else {
            done = true;
            myresult = -1;
          };
        };
      };
    };
    Result = myresult;
    return Result;
  };
  this.Initialiselinks = function (StartNode) {
    var i = 0;
    var targetNode = null;
    var myLink = null;
    if ($mod.TInterfaceObject.isPrototypeOf(StartNode)) if (StartNode.FLink !== null) {
      myLink = StartNode.FLink;
      if (myLink.FTIObjectName !== "") {
        targetNode = $mod.FindDataNodeById($mod.SystemNodeTree,myLink.FTIObjectName,false);
        if (targetNode !== null) {
          myLink.FTIObject = targetNode}
         else pas.StringUtils.ShowMessage("Initialiselinks.  Node is nil. " + myLink.FTIObjectName);
      };
    };
    for (var $l1 = 0, $end2 = rtl.length(StartNode.ChildNodes) - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      $mod.Initialiselinks(StartNode.ChildNodes[i]);
    };
  };
  this.DeleteNodeChildren = function (ParentNode) {
    var i = 0;
    if (ParentNode === null) {
      pas.StringUtils.ShowMessage("parentnode nil in DeleteNodeChildren")}
     else {
      for (var $l1 = 0, $end2 = rtl.length(ParentNode.ChildNodes) - 1; $l1 <= $end2; $l1++) {
        i = $l1;
        $mod.DeleteNodeChildren(ParentNode.ChildNodes[i]);
        $impl.DeleteScreenObject(ParentNode.ChildNodes[i]);
        ParentNode.ChildNodes[i].DeleteMe();
      };
      ParentNode.ChildNodes = rtl.arraySetLength(ParentNode.ChildNodes,null,0);
    };
  };
  this.InsertSystemNode = function (ParentNode, SourceNode, Position) {
    var Result = null;
    var myparent = null;
    var myself = null;
    var i = 0;
    var fn = null;
    myparent = ParentNode;
    if ((myparent !== null) && (myparent.NodeName !== "")) {
      fn = $mod.LookupComponentFunc(SourceNode.NodeType);
      if (fn !== null) {
        myself = $mod.CreateInterfaceObject(ParentNode.MyForm,SourceNode.NodeType,SourceNode.NodeName);
        for (var $l1 = 0, $end2 = rtl.length(SourceNode.NodeAttributes) - 1; $l1 <= $end2; $l1++) {
          i = $l1;
          myself.SetAttributeValue$1(SourceNode.NodeAttributes[i].AttribName,SourceNode.NodeAttributes[i].AttribValue);
        };
        myself = fn(myself,ParentNode,SourceNode.NodeName,Position);
        myself.IsDynamic = true;
        myself.SetAttributeValue$1("ParentName",ParentNode.NodeName);
        $mod.AddChildToParentNode({get: function () {
            return myparent;
          }, set: function (v) {
            myparent = v;
          }},{get: function () {
            return myself;
          }, set: function (v) {
            myself = v;
          }},Position);
        for (var $l3 = 0, $end4 = rtl.length(SourceNode.ChildNodes) - 1; $l3 <= $end4; $l3++) {
          i = $l3;
          $mod.InsertSystemNode(myself,SourceNode.ChildNodes[i],-1);
        };
      } else {
        pas.StringUtils.ShowMessage("No function defined to instantiate component of type " + SourceNode.NodeType);
        myself = null;
      };
    };
    Result = myself;
    return Result;
  };
  this.ClearAttribs = function (AttrParams) {
    AttrParams.set(rtl.arraySetLength(AttrParams.get(),$mod.TNodeAttribute,0));
  };
  this.NodeNameIsUnique = function (NodeName, showerror) {
    var Result = false;
    var myresult = false;
    var founditem = null;
    myresult = true;
    founditem = $mod.FindDataNodeById($mod.SystemNodeTree,NodeName,false);
    if ((founditem !== null) && (founditem.NodeName === NodeName)) {
      if (showerror) pas.StringUtils.ShowMessage(("Error. Name >" + NodeName) + "< is not unique when creating a new object");
      myresult = false;
    };
    Result = myresult;
    return Result;
  };
  this.InitSystemNodetree = function () {
    $mod.SystemNodeTree = $mod.TDataNode.$create("Create$1",["Root","ApplicationRoot","Root",false]);
    $mod.UIRootNode = $mod.TDataNode.$create("Create$1",["Root",$mod.SystemRootName,"Root",false]);
    $mod.AddChildToParentNode({p: $mod, get: function () {
        return this.p.SystemNodeTree;
      }, set: function (v) {
        this.p.SystemNodeTree = v;
      }},{p: $mod, get: function () {
        return this.p.UIRootNode;
      }, set: function (v) {
        this.p.UIRootNode = v;
      }},-1);
  };
  this.ClearAllDynamicNodes = function (StartNode) {
    var i = 0;
    for (var $l1 = rtl.length(StartNode.ChildNodes) - 1; $l1 >= 0; $l1--) {
      i = $l1;
      $mod.ClearAllDynamicNodes(StartNode.ChildNodes[i]);
      if (StartNode.ChildNodes[i].IsDynamic) $mod.DeleteNode(StartNode,StartNode.ChildNodes[i]);
    };
  };
  this.InitFormObject = function (myForm, NodeName) {
    var myNode = null;
    myForm.fName = NodeName;
    myNode = $mod.AddFormToNodeTree(myForm);
  };
  this.AddNodeFuncLookup = function (NodeType, InObFuncPtr, ScreenObjFunc) {
    var myRec = new $mod.TNodeFuncsLookup();
    var l = 0;
    if (NodeType === "") return;
    l = rtl.length($mod.NodeFuncsLookup);
    $mod.NodeFuncsLookup = rtl.arraySetLength($mod.NodeFuncsLookup,$mod.TNodeFuncsLookup,l + 1);
    myRec.NodeType = NodeType;
    myRec.InObFunctionPtr = InObFuncPtr;
    myRec.ScreenObjFunctionPtr = ScreenObjFunc;
    $mod.NodeFuncsLookup[l] = new $mod.TNodeFuncsLookup(myRec);
  };
  this.PushTolinks = function (AObject, PropName, PropValue, StartNode) {
    var i = 0;
    var MyPropType = 0;
    if ($mod.TInterfaceObject.isPrototypeOf(StartNode)) if (StartNode.FLink !== null) if ((StartNode.FLink.FTIObject === AObject) && (StartNode.FLink.FTIPropertyName === PropName)) {
      MyPropType = pas.TypInfo.PropType(AObject,PropName);
      if (MyPropType === pas.TypInfo.TTypeKind.tkString) {
        pas.TypInfo.SetStringProp(AObject,PropName,PropValue)}
       else if (MyPropType === pas.TypInfo.TTypeKind.tkBool) {
        pas.TypInfo.SetBoolProp(AObject,PropName,pas.StringUtils.MyStrToBool(PropValue))}
       else pas.StringUtils.ShowMessage("PushTolinks.  Need to handle property type for " + PropName);
    };
    for (var $l1 = 0, $end2 = rtl.length(StartNode.ChildNodes) - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      $mod.PushTolinks(AObject,PropName,PropValue,StartNode.ChildNodes[i]);
    };
  };
  this.SetInterfaceProperty = function (myName, PropName, NewValue) {
    var myObj = null;
    var MyPropType = 0;
    myObj = $mod.FindDataNodeById($mod.SystemNodeTree,myName,false);
    if (myObj !== null) {
      MyPropType = pas.TypInfo.PropType(myObj,PropName);
      if (MyPropType === pas.TypInfo.TTypeKind.tkString) {
        pas.TypInfo.SetStringProp(myObj,PropName,NewValue);
      } else if (MyPropType === pas.TypInfo.TTypeKind.tkBool) {
        pas.TypInfo.SetBoolProp(myObj,PropName,pas.StringUtils.MyStrToBool(NewValue));
      } else pas.StringUtils.ShowMessage("SetInterfaceProperty.  Need to handle property type for " + PropName);
    };
  };
  this.MainForm = null;
  this.SuppressEvents = false;
  this.TMethod = function (s) {
    if (s) {
      this.Code = s.Code;
      this.Data = s.Data;
    } else {
      this.Code = null;
      this.Data = null;
    };
    this.$equal = function (b) {
      return (this.Code === b.Code) && (this.Data === b.Data);
    };
  };
  this.XMLToNodeTree = function (XMLString) {
    var Result = "";
    var i = 0;
    var TempChar = "";
    var NextChar = "";
    var NewString = "";
    var BracesToggleFlag = false;
    var StringList = null;
    var RootNodeName = "";
    if ($impl.checkData(XMLString) === true) {
      StringList = pas.StringUtils.stringsplit(XMLString,$mod.delimiterBetweenTheEventHistoriesAndSystemDescription);
      $mod.StartingUp = true;
      RootNodeName = $mod.SystemRootName;
      var ob=document.getElementById(RootNodeName);
      $mod.SystemNodeTree.ScreenObject=ob;
      NewString = "";
      for (var $l1 = 1, $end2 = XMLString.length; $l1 <= $end2; $l1++) {
        i = $l1;
        TempChar = XMLString.charAt(i - 1);
        if (TempChar === "<") {
          TempChar = "";
          BracesToggleFlag = true;
          NextChar = XMLString.charAt((i + 1) - 1);
          if (NextChar === "\/") BracesToggleFlag = false;
        };
        if (TempChar === ">") {
          if (BracesToggleFlag === true) {
            $impl.addComponentFromXML(NewString);
          };
          BracesToggleFlag = false;
          NewString = "";
        };
        if (BracesToggleFlag === true) {
          NewString = NewString + TempChar;
        };
      };
      $mod.Initialiselinks($mod.SystemNodeTree);
      $mod.StartingUp = false;
    } else pas.StringUtils.ShowMessage("Error .....Unable to load data");
    return Result;
  };
  this.NilScreenObject = function (MyNode) {
    var i = 0;
    if (MyNode.ScreenObject !== null) {
      MyNode.ScreenObject = null;
    };
    for (var $l1 = 0, $end2 = rtl.length(MyNode.ChildNodes) - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      $mod.NilScreenObject(MyNode.ChildNodes[i]);
    };
  };
  this.CreateInterfaceObject = function (MyForm, NodeType, NodeName) {
    var Result = null;
    var myObj = null;
    var inobFn = null;
    var mynode = null;
    inobFn = $impl.LookupNodeInObFunc(NodeType);
    if (inobFn === null) {
      pas.StringUtils.ShowMessage("no interface object creation function for " + mynode.NodeName)}
     else {
      myObj = inobFn(MyForm,NodeName);
    };
    Result = myObj;
    return Result;
  };
  this.LoadedSystemString = "";
  this.SystemRootName = "UIRootNode";
  this.PortraitMode = false;
  this.SystemNodeTree = null;
  this.UIRootNode = null;
  this.StartingUp = false;
  this.loadingSystem = false;
  this.NodeFuncsLookup = [];
  this.EventAttributeDelimiter = "|@|";
  this.EventListdelimiter = "|^|";
  this.EventHistorydelimiter = "|~|";
  this.delimiterBetweenTheEventHistoriesAndSystemDescription = "|@@|";
  this.attributeListdelimiter = "|;";
  this.NameValuePairdelimiter = "|=";
  this.AttribBitsDelimiter = "|{";
  this.AttribLinkDelimiter = "|}";
  this.RegisteredEventsDelimiter = "|^^|";
  this.StartXMLString = "<";
  this.ToggleFlagMarker = "\/";
  this.EndXMLString = ">";
  $mod.$init = function () {
    $mod.InitSystemNodetree();
    $mod.SuppressEvents = false;
  };
},["WrapperPanel","Popup"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.ScanChildrenForNode = function (CurrentItem, ScreenObjectID, FoundParent) {
    var Result = null;
    var FoundItem = null;
    var TempItem = null;
    var TempArrayOfChildren = [];
    var NumChildren = 0;
    var i = 0;
    FoundItem = null;
    FoundParent.set(null);
    if (pas.SysUtils.Trim(pas.SysUtils.UpperCase(CurrentItem.NodeName)) === pas.SysUtils.Trim(pas.SysUtils.UpperCase(ScreenObjectID))) {
      FoundItem = CurrentItem;
    } else {
      TempArrayOfChildren = CurrentItem.ChildNodes;
      NumChildren = rtl.length(TempArrayOfChildren);
      i = 0;
      while (i < NumChildren) {
        if (FoundItem === null) {
          TempItem = CurrentItem.ChildNodes[i];
          if (pas.SysUtils.Trim(pas.SysUtils.UpperCase(TempItem.NodeName)) === pas.SysUtils.Trim(pas.SysUtils.UpperCase(ScreenObjectID))) {
            FoundItem = TempItem;
            FoundParent.set(CurrentItem);
            i = NumChildren;
          } else FoundItem = $impl.ScanChildrenForNode(TempItem,ScreenObjectID,FoundParent);
        };
        i = i + 1;
      };
    };
    Result = FoundItem;
    return Result;
  };
  $impl.MakeAttrib = function (attrName, attrType, attrValue, attrReadOnly) {
    var Result = new $mod.TNodeAttribute();
    var newAttrib = new $mod.TNodeAttribute();
    newAttrib.AttribName = attrName;
    newAttrib.AttribType = attrType;
    newAttrib.AttribValue = attrValue;
    newAttrib.AttribReadOnly = attrReadOnly;
    Result = new $mod.TNodeAttribute(newAttrib);
    return Result;
  };
  $impl.SubstituteSpecials = function (instring) {
    var Result = "";
    var tempstr = "";
    tempstr = instring;
    tempstr = pas.StringUtils.myStringReplace(tempstr,"<","&lt;",9999,9999);
    tempstr = pas.StringUtils.myStringReplace(tempstr,">","&gt;",9999,9999);
    tempstr = pas.StringUtils.myStringReplace(tempstr,"'","&apos;",9999,9999);
    Result = tempstr;
    return Result;
  };
  $impl.UnSubstituteSpecials = function (instring) {
    var Result = "";
    var tempstr = "";
    tempstr = instring;
    tempstr = pas.StringUtils.myStringReplace(tempstr,"&lt;","<",9999,9999);
    tempstr = pas.StringUtils.myStringReplace(tempstr,"&gt;",">",9999,9999);
    tempstr = pas.StringUtils.myStringReplace(tempstr,"&apos;","'",9999,9999);
    Result = tempstr;
    return Result;
  };
  $impl.AttribsFromXML = function (attributeList, offset, ParentName, NewLink) {
    var Result = [];
    var myAttribs = [];
    var LinkSet = null;
    var AttribBits = null;
    var i = 0;
    myAttribs = rtl.arraySetLength(myAttribs,$mod.TNodeAttribute,attributeList.GetCount() - offset);
    for (var $l1 = offset, $end2 = attributeList.GetCount() - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      AttribBits = pas.StringUtils.stringsplit(attributeList.Get(i),$mod.AttribBitsDelimiter);
      myAttribs[i - offset].AttribName = pas.StringUtils.TrimWhiteSpace(AttribBits.Get(0));
      myAttribs[i - offset].AttribType = pas.StringUtils.TrimWhiteSpace(AttribBits.Get(1));
      myAttribs[i - offset].AttribValue = $impl.UnSubstituteSpecials(AttribBits.Get(2));
      myAttribs[i - offset].AttribReadOnly = pas.StringUtils.MyStrToBool(pas.StringUtils.TrimWhiteSpace(AttribBits.Get(3)));
      if (myAttribs[i - offset].AttribName === "ParentName") ParentName.set(AttribBits.Get(2));
      if (myAttribs[i - offset].AttribName === "Link") {
        LinkSet = pas.StringUtils.stringsplit(myAttribs[i - offset].AttribValue,$mod.AttribLinkDelimiter);
        if (LinkSet.GetCount() > 1) {
          NewLink.set($mod.TXPropertyLink.$create("Create"));
          NewLink.get().FTIObjectName = LinkSet.Get(0);
          NewLink.get().FTIPropertyName = LinkSet.Get(1);
        };
      };
    };
    Result = myAttribs;
    return Result;
  };
  $impl.LinkToStr = function (ALink) {
    var Result = "";
    Result = ALink.FTIObjectName;
    Result = (Result + $mod.AttribLinkDelimiter) + ALink.FTIPropertyName;
    return Result;
  };
  $impl.LookupNodeInObFunc = function (NodeType) {
    var Result = null;
    var i = 0;
    i = 0;
    while (i < rtl.length($mod.NodeFuncsLookup)) {
      if ($mod.NodeFuncsLookup[i].NodeType === NodeType) {
        Result = $mod.NodeFuncsLookup[i].InObFunctionPtr;
        i = rtl.length($mod.NodeFuncsLookup);
      };
      i = i + 1;
    };
    return Result;
  };
  $impl.addComponentFromXML = function (XMLString) {
    var Result = "";
    var ParentName = "";
    var ScreenObjectName = "";
    var ClassName = "";
    var ScreenObjectType = "";
    var mf = "";
    var attributeList = null;
    var NameValuePair = null;
    var i = 0;
    var myAttribs = [];
    var ParentNode = null;
    var mynode = null;
    var NodeString = "";
    var fn = null;
    var NewLink = null;
    var myDynamicWrapper = null;
    NodeString = XMLString;
    attributeList = pas.StringUtils.stringsplit(NodeString,$mod.attributeListdelimiter);
    ScreenObjectType = attributeList.Get(0);
    NameValuePair = pas.StringUtils.stringsplit(attributeList.Get(1),$mod.NameValuePairdelimiter);
    ClassName = pas.StringUtils.TrimWhiteSpace(NameValuePair.Get(1));
    NameValuePair = pas.StringUtils.stringsplit(attributeList.Get(2),$mod.NameValuePairdelimiter);
    ScreenObjectName = pas.StringUtils.TrimWhiteSpace(NameValuePair.Get(1));
    if ((ScreenObjectName !== $mod.SystemRootName) && (ClassName !== "Root")) {
      myAttribs = $impl.AttribsFromXML(attributeList,3,{get: function () {
          return ParentName;
        }, set: function (v) {
          ParentName = v;
        }},{get: function () {
          return NewLink;
        }, set: function (v) {
          NewLink = v;
        }});
      if (((ClassName === "UI") && (ScreenObjectType !== "")) || (ClassName === "SVG")) {
        if (ParentName === "") {
          ParentNode = $mod.SystemNodeTree}
         else ParentNode = $mod.FindDataNodeById($mod.SystemNodeTree,ParentName,true);
        mynode = $mod.FindDataNodeById($mod.SystemNodeTree,ScreenObjectName,false);
        if (mynode === null) {
          myDynamicWrapper = $mod.CreateInterfaceObject(ParentNode.MyForm,ScreenObjectType,ScreenObjectName);
          if (myDynamicWrapper !== null) {
            mynode = myDynamicWrapper.myNode;
            mynode.IsDynamic = true;
          };
        };
        if (mynode === null) pas.StringUtils.ShowMessage("oops node still nil");
        $mod.ReParentNode(mynode,ParentNode);
        for (var $l1 = 0, $end2 = rtl.length(myAttribs) - 1; $l1 <= $end2; $l1++) {
          i = $l1;
          mynode.SetAttributeValue$1(myAttribs[i].AttribName,myAttribs[i].AttribValue);
        };
        if ($mod.TInterfaceObject.isPrototypeOf(mynode)) mynode.SetLink(NewLink);
        mf = $mod.MainForm.fName;
        if (ScreenObjectName!=mf) {
          // object may already exist if this is a system re-load, so delete the old one.
          var ob = document.getElementById(ScreenObjectName);
          if (ob!=null) {
            //alert('found '+ScreenObjectName);
            var Parent = pas.HTMLUtils.ScreenObjectInnerComponent(ParentNode);
            if (Parent!=null) {
              //alert('deleting '+ScreenObjectName);
              Parent.removeChild(ob); }
          }
        };
        fn = $mod.LookupComponentFunc(ScreenObjectType);
        fn(mynode,ParentNode,ScreenObjectName,-1);
      };
    };
    return Result;
  };
  $impl.DeleteScreenObject = function (MyNode) {
    var Result = "";
    var ObjName = "";
    ObjName = MyNode.NodeName;
    try{
    var ThisObject = document.getElementById(ObjName);
    if (ThisObject!=null) {
       ThisObject.parentNode.removeChild(ThisObject);
      }
    }catch(err) { alert(err.message+' in NodeUtils.DeleteScreenObject');};
    $mod.NilScreenObject(MyNode);
    return Result;
  };
  $impl.checkData = function (SystemDescription) {
    var Result = false;
    var teststring = "";
    var i = 0;
    var MatchFound = false;
    MatchFound = true;
    teststring = "<Root|; Class |=R";
    for (var $l1 = 1, $end2 = teststring.length; $l1 <= $end2; $l1++) {
      i = $l1;
      if (SystemDescription.charAt(i - 1) !== teststring.charAt(i - 1)) MatchFound = false;
    };
    Result = MatchFound;
    return Result;
  };
});
rtl.module("XScrollBox",["System","Classes","SysUtils","NodeUtils","WrapperPanel"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TXScrollBox",pas.WrapperPanel.TWrapperPanel,function () {
    this.$init = function () {
      pas.WrapperPanel.TWrapperPanel.$init.call(this);
      this.FAlignChildrenVertical$1 = false;
    };
    this.SetMyEventTypes = function () {
      this.myEventTypes.Add("Click");
    };
    this.GetScrollType = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("ScrollType",true).AttribValue;
      return Result;
    };
    this.GetAlignment$1 = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("Alignment",true).AttribValue;
      return Result;
    };
    this.SetScrollType = function (AValue) {
      var AVal = "";
      this.myNode.SetAttributeValue$1("ScrollType",AValue);
      AVal = pas.SysUtils.UpperCase(AValue);
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
        ob.style.overlow='none';
        if ((AVal=='BOTH')||(AVal=='RIGHT')) {ob.style.overflowY='scroll';}
        if ((AVal=='BOTH')||(AVal=='BOTTOM')) {ob.style.overflowX='scroll';}
        };
    };
    this.SetAlignment$1 = function (AValue) {
      if (this.myNode !== null) {
        this.myNode.SetAttributeValue$1("Alignment",AValue);
        this.SortOutAlignment();
      };
    };
    this.SortOutAlignment = function () {
      var ParentAlignChildrenVertical = false;
      var MyAlignment = "";
      var MyLabelPos = "";
      var ParentNode = null;
      MyAlignment = this.GetAlignment$1();
      MyLabelPos = this.GetLabelPos();
      ParentNode = pas.NodeUtils.FindParentOfNode$1(pas.NodeUtils.SystemNodeTree,this.NodeName);
      ParentAlignChildrenVertical = ParentNode.FAlignChildrenVertical;
      if ((MyAlignment === "Right") || (MyAlignment === "Left")) {
        if (ParentAlignChildrenVertical === false) MyAlignment = "Top";
      } else if ((MyAlignment === "Top") || (MyAlignment === "Bottom")) {
        if (ParentAlignChildrenVertical === true) MyAlignment = "Left";
      };
      try {
             var ob = document.getElementById(this.NodeName+'Contents');
             var wrapper = document.getElementById(this.NodeName);
      
             if ((ob!=null)  && (wrapper!=null)) {
             wrapper.classList.remove('hboxNoStretch');
             wrapper.classList.remove('vboxNoStretch');
             wrapper.classList.remove('AlignmentCentre');
             wrapper.classList.remove('AlignmentRight');
             wrapper.classList.remove('AlignmentLeft');
             wrapper.classList.remove('AlignmentTop');
             wrapper.classList.remove('AlignmentBottom');
      
             if (MyAlignment=='Right') {
               if (ParentAlignChildrenVertical) {
               ob.style.float='right';
               wrapper.classList.add('AlignmentRight');
             }
             }
             else if (MyAlignment=='Left') {
             if (ParentAlignChildrenVertical) {
                 ob.style.float='left';
                 wrapper.classList.add('AlignmentLeft');
               }
               }
             else if (MyAlignment=='Centre') {
               ob.style.float='left';
                wrapper.classList.add('AlignmentCentre');
             }
      
             else if (MyAlignment=='Top') {
             if (ParentAlignChildrenVertical==false) {
               ob.style.float='left';
               wrapper.classList.add('AlignmentTop');
             }
             }
             else if (MyAlignment=='Bottom') {
             if (ParentAlignChildrenVertical==false) {
               ob.style.float='left';
               wrapper.classList.add('AlignmentBottom');
             }
           }
      
         }
       } catch(err) { alert(err.message+'  in XScrollBox.SortOutAlignment'); };
    };
    this.Create$3 = function (MyForm, NodeName) {
      pas.WrapperPanel.TWrapperPanel.Create$2.call(this,NodeName);
      this.NodeType = $impl.MyNodeType;
      this.MyForm = MyForm;
      this.FAlignChildrenVertical$1 = true;
      this.SetMyEventTypes();
      this.FIsContainer = true;
      this.SetBgColor("#FFFFFF");
      this.SetmyWidth("300px");
      this.SetmyHeight("300px");
      this.SetScrollType("Both");
    };
    var $r = this.$rtti;
    $r.addProperty("Alignment",3,rtl.string,"GetAlignment$1","SetAlignment$1");
    $r.addProperty("AlignChildrenVertical",0,rtl.boolean,"FAlignChildrenVertical$1","FAlignChildrenVertical$1");
    $r.addProperty("ScrollType",3,rtl.string,"GetScrollType","SetScrollType");
  });
  $mod.$init = function () {
    pas.NodeUtils.AddNodeFuncLookup($impl.MyNodeType,$impl.CreateinterfaceObj,$impl.CreateWidget);
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.MyNodeType = "TXScrollBox";
  $impl.CreateWidget = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    var ScrollType = "";
    var OnClickString = "";
    ScrollType = pas.SysUtils.UpperCase(MyNode.GetAttribute("ScrollType",true).AttribValue);
    OnClickString = ('onclick="event.stopPropagation();pas.Events.handleEvent(\'Click\',\'' + ScreenObjectName) + '\', this.value, \'\');" ';
    try{
    
        var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
        wrapper.style.overflow = 'hidden';
    
        var HTMLString='';
        var MyObjectName=ScreenObjectName+'Contents';
        var oflow = ''
        if ((ScrollType=='BOTH')||(ScrollType=='RIGHT')) {oflow = 'overflow-y:scroll; '}
        if ((ScrollType=='BOTH')||(ScrollType=='BOTTOM')) {oflow = oflow+'overflow-x:scroll; '}
    
        HTMLString = '<div id='+MyObjectName+ ' style="'+oflow+' height:100%; width:100%;" ' +
                     OnClickString +
                     '></div> ';
    
    
        var wrapper=document.getElementById(ScreenObjectName);
        wrapper.insertAdjacentHTML('beforeend', HTMLString);
    
      }
      catch(err) { alert(err.message+'  in XScrollBox.CreateWidget');};
    MyNode.ScreenObject = MyNode;
    MyNode.SetmyHeight(MyNode.GetmyHeight());
    MyNode.SetmyWidth(MyNode.GetmyWidth());
    MyNode.SetAlignment$1(MyNode.GetAlignment$1());
    MyNode.SetHint(MyNode.GetHint());
    Result = MyNode;
    return Result;
  };
  $impl.CreateinterfaceObj = function (MyForm, NodeName) {
    var Result = null;
    Result = $mod.TXScrollBox.$create("Create$3",[MyForm,NodeName]);
    return Result;
  };
});
rtl.module("XButton",["System","Classes","SysUtils","NodeUtils","StringUtils","HTMLUtils","WrapperPanel"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TXButton",pas.WrapperPanel.TWrapperPanel,function () {
    this.SetMyEventTypes = function () {
      this.myEventTypes.Add("ButtonClick");
    };
    this.GetmyCaption = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("myCaption",true).AttribValue;
      return Result;
    };
    this.GetEnabled = function () {
      var Result = false;
      Result = pas.StringUtils.MyStrToBool(this.myNode.GetAttribute("Enabled",true).AttribValue);
      return Result;
    };
    this.SetmyCaption = function (AValue) {
      this.myNode.SetAttributeValue$1("myCaption",AValue);
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
         ob.value=AValue;  };
    };
    this.SetEnabled = function (AValue) {
      this.myNode.SetAttributeValue("Enabled",pas.StringUtils.MyBoolToStr(AValue),"Boolean");
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
      if (AValue==false) {ob.disabled = true}
      else {ob.disabled = false }
      };
    };
    this.Create$3 = function (MyForm, NodeName) {
      pas.WrapperPanel.TWrapperPanel.Create$2.call(this,NodeName);
      this.NodeType = $impl.MyNodeType;
      this.MyForm = MyForm;
      this.SetMyEventTypes();
      this.FIsContainer = false;
      this.SetHint("");
      this.SetmyCaption("Press Me");
      this.SetEnabled(true);
    };
    var $r = this.$rtti;
    $r.addProperty("myCaption",3,rtl.string,"GetmyCaption","SetmyCaption");
    $r.addProperty("Enabled",3,rtl.boolean,"GetEnabled","SetEnabled");
  });
  $mod.$init = function () {
    pas.WrapperPanel.SuppressDesignerProperty("TXButton","LabelPos");
    pas.WrapperPanel.SuppressDesignerProperty("TXButton","LabelText");
    pas.WrapperPanel.SuppressDesignerProperty("TXButton","BgColor");
    pas.NodeUtils.AddNodeFuncLookup($impl.MyNodeType,$impl.CreateinterfaceObj,$impl.CreateWidget);
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.MyNodeType = "TXButton";
  $impl.CreateWidget = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    var xItemText = "";
    var xEnabled = "";
    var marginString = "";
    xItemText = MyNode.GetAttribute("myCaption",true).AttribValue;
    xEnabled = MyNode.GetAttribute("Enabled",true).AttribValue;
    marginString = ((((((("margin:" + pas.HTMLUtils.glbMarginSpacing) + " ") + pas.HTMLUtils.glbMarginSpacing) + " ") + pas.HTMLUtils.glbMarginSpacing) + " ") + pas.HTMLUtils.glbMarginSpacing) + ";";
    try{
      //alert('CreateWidget Button...');
        var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    
        var HTMLString='';
        var NodeIDString = "'"+ScreenObjectName+"'";
        var componentClick="'ComponentClick'";
        var MyObjectName=ScreenObjectName+'Contents';
    
        var EnabledString = '';
        if (xEnabled=='False') { EnabledString = ' disabled ';}
    
        var typestring="'ButtonClick'";
        var Caption="'"+xItemText+"'";
        var blankParam="''";
        HTMLString = '<input type="button" id='+MyObjectName+' style="display: inline-block; '+
                                                                     'width:100%; '+
                                                                      marginString+'" '+
                                                                      '" '+
        'onclick="event.stopPropagation(); pas.Events.handleEvent('+typestring+','+NodeIDString+', '+NodeIDString+','+blankParam+');"'+
                            '  '+EnabledString+' value="'+xItemText+'"> ';
    
        var wrapper=document.getElementById(ScreenObjectName);
        wrapper.insertAdjacentHTML('beforeend', HTMLString);
        // MyNode.ScreenObject=wrapper;
      }
      catch(err) { alert(err.message+'  in XButton.CreateWidget');};
    MyNode.ScreenObject = MyNode;
    MyNode.SetmyWidth(MyNode.GetmyWidth());
    MyNode.SetAlignment(MyNode.GetAlignment());
    MyNode.SetHint(MyNode.GetHint());
    Result = MyNode;
    return Result;
  };
  $impl.CreateinterfaceObj = function (MyForm, NodeName) {
    var Result = null;
    Result = $mod.TXButton.$create("Create$3",[MyForm,NodeName]);
    return Result;
  };
});
rtl.module("XEditBox",["System","Classes","SysUtils","TypInfo","NodeUtils","StringUtils","WrapperPanel"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TXEditBox",pas.WrapperPanel.TWrapperPanel,function () {
    this.SetMyEventTypes = function () {
      this.myEventTypes.Add("Click");
      this.myEventTypes.Add("Change");
      this.myEventTypes.Add("EditBoxPaste");
    };
    this.GetItemValue = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("ItemValue",true).AttribValue;
      return Result;
    };
    this.GetReadOnly = function () {
      var Result = false;
      Result = pas.StringUtils.MyStrToBool(this.myNode.GetAttribute("ReadOnly",true).AttribValue);
      return Result;
    };
    this.GetBoxWidth = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("BoxWidth",true).AttribValue;
      return Result;
    };
    this.SetItemValue = function (AValue) {
      this.myNode.SetAttributeValue$1("ItemValue",AValue);
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
         ob.value=AValue;  };
      this.LinkSaveToProperty(this);
    };
    this.SetReadOnly = function (AValue) {
      this.myNode.SetAttributeValue("ReadOnly",pas.StringUtils.MyBoolToStr(AValue),"Boolean");
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
        ob.readOnly = AValue  };
    };
    this.SetBoxWidth = function (AValue) {
      this.myNode.SetAttributeValue$1("BoxWidth",AValue);
      var ob = document.getElementById(this.NodeName+'Contents');
      //  if (ob==null) {alert(this.NodeName+'Contents'+'  not found');}
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
    };
    this.LinkLoadFromProperty = function (Sender) {
      pas.NodeUtils.TInterfaceObject.LinkLoadFromProperty.call(this,Sender);
    };
    this.LinkSaveToProperty = function (Sender) {
      if (Sender === null) ;
      if (this.FLink === null) return;
      if (this.FLink.FTIObject === null) return;
      pas.TypInfo.SetStringProp(this.FLink.FTIObject,this.FLink.FTIPropertyName,this.GetItemValue());
    };
    this.Create$3 = function (MyForm, NodeName) {
      pas.WrapperPanel.TWrapperPanel.Create$2.call(this,NodeName);
      this.NodeType = $impl.MyNodeType;
      this.MyForm = MyForm;
      this.SetMyEventTypes();
      this.FIsContainer = false;
      this.SetHint("");
      this.SetBoxWidth("200");
      this.SetLabelText("Edit Box");
      this.SetLabelPos("Right");
      this.SetItemValue("");
      this.SetReadOnly(false);
    };
    var $r = this.$rtti;
    $r.addMethod("LinkLoadFromProperty",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("LinkSaveToProperty",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addProperty("ItemValue",3,rtl.string,"GetItemValue","SetItemValue");
    $r.addProperty("ReadOnly",3,rtl.boolean,"GetReadOnly","SetReadOnly");
    $r.addProperty("BoxWidth",3,rtl.string,"GetBoxWidth","SetBoxWidth");
  });
  $mod.$init = function () {
    pas.WrapperPanel.SuppressDesignerProperty("TXEditBox","BgColor");
    pas.WrapperPanel.SuppressDesignerProperty("TXEditBox","myHeight");
    pas.NodeUtils.AddNodeFuncLookup($impl.MyNodeType,$impl.CreateinterfaceObj,$impl.CreateWidget);
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.MyNodeType = "TXEditBox";
  $impl.CreateWidget = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    var ItemValue = "";
    var LabelText = "";
    var ReadOnly = false;
    var OnChangeString = "";
    var OnClickString = "";
    var OnPasteString = "";
    ItemValue = MyNode.GetAttribute("ItemValue",true).AttribValue;
    LabelText = MyNode.GetAttribute("LabelText",true).AttribValue;
    ReadOnly = pas.SysUtils.StrToBool(MyNode.GetAttribute("ReadOnly",true).AttribValue);
    OnClickString = ('onclick="event.stopPropagation();pas.Events.handleEvent(\'Click\',\'' + ScreenObjectName) + '\', this.value,\'\');" ';
    OnChangeString = ((('onchange="pas.NodeUtils.SetInterfaceProperty(\'' + ScreenObjectName) + "','ItemValue',this.value); pas.Events.handleEvent('Change','") + ScreenObjectName) + '\', this.value, \'ItemValue\');" ';
    OnPasteString = ('onpaste="pas.Events.handleEvent(\'EditBoxPaste\',\'' + ScreenObjectName) + '\', this.value,\'\');" ';
    try{
        var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
        wrapper.style.overflow = 'hidden';
    
        var HTMLString='';
        var NodeIDString = "'"+ScreenObjectName+"'";
        var componentClick="'Click'";
        var MyObjectName=ScreenObjectName+'Contents';
    
        var ReadOnlyString = '';
        if (ReadOnly==true) { ReadOnlyString = ' readonly ';}
    
        var inputtext= ItemValue;
        var Pastetypestring="'EditBoxPaste'";
        var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';
        var EBoxString = '<input type="text"  id='+MyObjectName+' ' +
                              OnPasteString +
                              OnClickString +
                              OnChangeString +
                     ' style="display: inline-block; '+
                     '" value="'+inputtext+'"'+ReadOnlyString+'>' ;
    
        HTMLString = labelstring+EBoxString;
    
        var wrapper=document.getElementById(ScreenObjectName);
        wrapper.insertAdjacentHTML('beforeend', HTMLString);
        //MyNode.ScreenObject=wrapper;
    
        // fix the height for an edit box to one line-height...
        var ob=document.getElementById(MyObjectName);
        var obStyle = window.getComputedStyle(ob);
        ob.style.maxHeight = obStyle.getPropertyValue('line-height');
        //alert('maxHeight='+ob.style.maxHeight);
      }
      catch(err) { alert(err.message+'  in XEditBox.CreateXEditBox');};
    MyNode.ScreenObject = MyNode;
    MyNode.SetBoxWidth(MyNode.GetBoxWidth());
    MyNode.SetAlignment(MyNode.GetAlignment());
    MyNode.SetLabelPos(MyNode.GetLabelPos());
    MyNode.SetHint(MyNode.GetHint());
    Result = MyNode;
    return Result;
  };
  $impl.CreateinterfaceObj = function (MyForm, NodeName) {
    var Result = null;
    Result = $mod.TXEditBox.$create("Create$3",[MyForm,NodeName]);
    return Result;
  };
});
rtl.module("XCheckBox",["System","Classes","SysUtils","TypInfo","NodeUtils","StringUtils","WrapperPanel"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TXCheckBox",pas.WrapperPanel.TWrapperPanel,function () {
    this.SetMyEventTypes = function () {
      this.myEventTypes.Add("Click");
    };
    this.GetChecked = function () {
      var Result = false;
      Result = pas.StringUtils.MyStrToBool(this.myNode.GetAttribute("Checked",true).AttribValue);
      return Result;
    };
    this.GetReadOnly = function () {
      var Result = false;
      Result = pas.StringUtils.MyStrToBool(this.myNode.GetAttribute("ReadOnly",true).AttribValue);
      return Result;
    };
    this.SetChecked = function (AValue) {
      this.myNode.SetAttributeValue("Checked",pas.StringUtils.MyBoolToStr(AValue),"Boolean");
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
         ob.checked=AValue;  };
      this.LinkSaveToProperty(this);
    };
    this.SetReadOnly = function (AValue) {
      this.myNode.SetAttributeValue("ReadOnly",pas.StringUtils.MyBoolToStr(AValue),"Boolean");
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
      if (AValue==true) {ob.disabled = true}
      else {ob.disabled = false }  };
    };
    this.LinkLoadFromProperty = function (Sender) {
      pas.NodeUtils.TInterfaceObject.LinkLoadFromProperty.call(this,Sender);
    };
    this.LinkSaveToProperty = function (Sender) {
      if (Sender === null) ;
      if (this.FLink === null) return;
      if (this.FLink.FTIObject === null) return;
      pas.TypInfo.SetBoolProp(this.FLink.FTIObject,this.FLink.FTIPropertyName,this.GetChecked());
    };
    this.Create$3 = function (MyForm, NodeName) {
      pas.WrapperPanel.TWrapperPanel.Create$2.call(this,NodeName);
      this.NodeType = $impl.MyNodeType;
      this.MyForm = MyForm;
      this.SetMyEventTypes();
      this.FIsContainer = false;
      this.SetHint("");
      this.SetmyWidth("");
      this.SetLabelText("Edit Box");
      this.SetLabelPos("Right");
      this.SetChecked(false);
      this.SetReadOnly(false);
    };
    var $r = this.$rtti;
    $r.addMethod("LinkLoadFromProperty",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("LinkSaveToProperty",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addProperty("Checked",3,rtl.boolean,"GetChecked","SetChecked");
    $r.addProperty("ReadOnly",3,rtl.boolean,"GetReadOnly","SetReadOnly");
  });
  $mod.$init = function () {
    pas.WrapperPanel.SuppressDesignerProperty("TXCheckBox","BgColor");
    pas.NodeUtils.AddNodeFuncLookup($impl.MyNodeType,$impl.CreateinterfaceObj,$impl.CreateWidget);
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.MyNodeType = "TXCheckBox";
  $impl.CreateWidget = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    var Checked = "";
    var ReadOnly = "";
    var LabelText = "";
    var OnClickString = "";
    Checked = MyNode.GetAttribute("Checked",true).AttribValue;
    LabelText = MyNode.GetAttribute("LabelText",true).AttribValue;
    ReadOnly = MyNode.GetAttribute("ReadOnly",true).AttribValue;
    OnClickString = ((((('onclick="pas.NodeUtils.SetInterfaceProperty(\'' + ScreenObjectName) + "','Checked',this.checked.toString());") + "event.stopPropagation(); ") + "pas.Events.handleEvent('Click','") + ScreenObjectName) + '\', this.checked.toString(),\'\');"';
    try{
        var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
        wrapper.style.overflow = 'hidden';
        wrapper.style.overflow = 'hidden';
        wrapper.style.display = 'flex';
        var goright =  'flex-e'+'nd';
    
        var HTMLString='';
        var NodeIDString = "'"+ScreenObjectName+"'";
        var MyObjectName=ScreenObjectName+'Contents';
    
        var ReadOnlyString = '';
        if (ReadOnly=='True') { ReadOnlyString = ' readonly ';}
    
        var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';
    
        var Checkstring = '';
        if (Checked == 'true'){Checkstring = 'checked'};
    
        var CheckBoxString = '<input  type="checkbox" id='+MyObjectName+ ' '+
                           OnClickString +
                           Checkstring +
                           ' style="display:inline-block;" '+ReadOnlyString+' >' ;
    
        HTMLString = labelstring+CheckBoxString;
    
        var wrapper=document.getElementById(ScreenObjectName);
        wrapper.insertAdjacentHTML('beforeend', HTMLString);
        //MyNode.ScreenObject=wrapper;
      }
      catch(err) { alert(err.message+'  in XCheckBox.CreateXCheckBox');};
    MyNode.ScreenObject = MyNode;
    MyNode.SetAlignment(MyNode.GetAlignment());
    MyNode.SetLabelPos(MyNode.GetLabelPos());
    MyNode.SetHint(MyNode.GetHint());
    Result = MyNode;
    return Result;
  };
  $impl.CreateinterfaceObj = function (MyForm, NodeName) {
    var Result = null;
    Result = $mod.TXCheckBox.$create("Create$3",[MyForm,NodeName]);
    return Result;
  };
});
rtl.module("XHBox",["System","Classes","SysUtils","NodeUtils","StringUtils","WrapperPanel"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TXHBox",pas.WrapperPanel.TWrapperPanel,function () {
    this.SetMyEventTypes = function () {
      this.myEventTypes.Add("Click");
    };
    this.GetBorder = function () {
      var Result = false;
      Result = pas.StringUtils.MyStrToBool(this.myNode.GetAttribute("Border",true).AttribValue);
      return Result;
    };
    this.SetBorder = function (AValue) {
      this.myNode.SetAttributeValue("Border",pas.StringUtils.MyBoolToStr(AValue),"Boolean");
      var ob = document.getElementById(this.NodeName);
      if (ob!=null) {
      if (AValue==true ) {
         ob.classList.add("normal-border");
      }
      else {
         ob.classList.remove("normal-border");
      } };
    };
    this.Create$3 = function (MyForm, NodeName) {
      pas.WrapperPanel.TWrapperPanel.Create$2.call(this,NodeName);
      this.NodeType = $impl.MyNodeType;
      this.MyForm = MyForm;
      this.SetMyEventTypes();
      this.FAlignChildrenVertical = false;
      this.SetBgColor("#FFFFFF");
      this.SetmyWidth("");
      this.SetmyHeight("");
      this.SetBorder(true);
    };
    var $r = this.$rtti;
    $r.addProperty("Border",3,rtl.boolean,"GetBorder","SetBorder");
  });
  $mod.$init = function () {
    pas.NodeUtils.AddNodeFuncLookup($impl.MyNodeType,$impl.CreateinterfaceObj,$impl.CreateWidget);
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.MyNodeType = "TXHBox";
  $impl.CreateWidget = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    var ShowBorder = false;
    var Bdr = "";
    Bdr = MyNode.GetAttribute("Border",true).AttribValue;
    if (Bdr !== "") {
      ShowBorder = pas.StringUtils.MyStrToBool(Bdr)}
     else ShowBorder = false;
    try{
    
        var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    
        var HTMLString='';
        var NodeIDString = "'"+ScreenObjectName+"'";
        var componentClick="'ComponentClick'";
        var MyObjectName=ScreenObjectName+'Contents';
        var blankParam="''";
    
        HTMLString = '<div  id="'+MyObjectName+'" class="hbox" '+
                           //'style="'+Wd1+Ht1+'"'+
                           'style="height:100%;width:100%"'+
                           ' onclick="event.stopPropagation();pas.Events.handleEvent('+componentClick+','+NodeIDString+', this.value,'+blankParam+'); " '+
                            '></div>  ';
    
        var wrapper=document.getElementById(ScreenObjectName);
        wrapper.insertAdjacentHTML('beforeend', HTMLString);
    
        }catch(err) { alert(err.message+'  in XHBox.CreateHBox');};
    MyNode.ScreenObject = MyNode;
    MyNode.SetmyHeight(MyNode.GetmyHeight());
    MyNode.SetmyWidth(MyNode.GetmyWidth());
    MyNode.SetAlignment(MyNode.GetAlignment());
    MyNode.SetHint(MyNode.GetHint());
    Result = MyNode;
    return Result;
  };
  $impl.CreateinterfaceObj = function (MyForm, NodeName) {
    var Result = null;
    Result = $mod.TXHBox.$create("Create$3",[MyForm,NodeName]);
    return Result;
  };
});
rtl.module("XHyperLink",["System","Classes","SysUtils","NodeUtils","WrapperPanel"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TXHyperLink",pas.WrapperPanel.TWrapperPanel,function () {
    this.SetMyEventTypes = function () {
      this.myEventTypes.Add("Click");
    };
    this.GetLabelCaption = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("LabelCaption",true).AttribValue;
      return Result;
    };
    this.GetURL = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("URL",true).AttribValue;
      return Result;
    };
    this.SetLabelCaption = function (AValue) {
      this.myNode.SetAttributeValue$1("LabelCaption",AValue);
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
         ob.innerHTML=AValue;   };
    };
    this.SetURL = function (AValue) {
      this.myNode.SetAttributeValue$1("URL",AValue);
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
         ob.href=AValue;   };
    };
    this.Create$3 = function (MyForm, NodeName) {
      pas.WrapperPanel.TWrapperPanel.Create$2.call(this,NodeName);
      this.NodeType = $impl.MyNodeType;
      this.MyForm = MyForm;
      this.SetMyEventTypes();
      this.FIsContainer = false;
      this.SetHint("");
      this.SetLabelCaption("BBC News");
      this.SetURL("http:\/\/www.bbc.co.uk\/news");
    };
    var $r = this.$rtti;
    $r.addProperty("LabelCaption",3,rtl.string,"GetLabelCaption","SetLabelCaption");
    $r.addProperty("URL",3,rtl.string,"GetURL","SetURL");
  });
  $mod.$init = function () {
    pas.NodeUtils.AddNodeFuncLookup($impl.MyNodeType,$impl.CreateinterfaceObj,$impl.CreateWidget);
    pas.WrapperPanel.SuppressDesignerProperty("TXHyperLink","BgColor");
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.MyNodeType = "TXHyperlink";
  $impl.CreateWidget = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    var LabelCaption = "";
    var URL = "";
    var OnClickString = "";
    LabelCaption = MyNode.GetAttribute("LabelCaption",true).AttribValue;
    URL = MyNode.GetAttribute("URL",true).AttribValue;
    OnClickString = ((('onclick="event.stopPropagation();pas.Events.handleEvent(\'Click\',\'' + ScreenObjectName) + "','") + URL) + '\',\'\'); " ';
    try{
        var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
        wrapper.style.overflow = 'hidden';
    
        var HTMLString='';
        var MyObjectName=ScreenObjectName+'Contents';
    
        HTMLString = '<a id='+MyObjectName+' href="'+URL+'" target="_blank" '+
                             OnClickString +
                             ' style="display: inline-block;"  >'+LabelCaption+'</a> ';
    
        var wrapper=document.getElementById(ScreenObjectName);
        wrapper.insertAdjacentHTML('beforeend', HTMLString);
        //MyNode.ScreenObject=wrapper;
      }
      catch(err) { alert(err.message+'  in XHyperLink.CreateWidget');};
    MyNode.ScreenObject = MyNode;
    MyNode.SetAlignment(MyNode.GetAlignment());
    MyNode.SetHint(MyNode.GetHint());
    Result = MyNode;
    return Result;
  };
  $impl.CreateinterfaceObj = function (MyForm, NodeName) {
    var Result = null;
    Result = $mod.TXHyperLink.$create("Create$3",[MyForm,NodeName]);
    return Result;
  };
});
rtl.module("XTabControl",["System","Classes","SysUtils","NodeUtils","HTMLUtils","WrapperPanel"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.ChangeTabPage = function (nodeId) {
    var Result = "";
    var ParentNode = null;
    ParentNode = pas.NodeUtils.FindParentOfNode$1(pas.NodeUtils.SystemNodeTree,nodeId);
    $impl.openTab(nodeId,ParentNode.NodeName + "Contents");
    return Result;
  };
  rtl.createClass($mod,"TXTabControl",pas.WrapperPanel.TWrapperPanel,function () {
    this.$init = function () {
      pas.WrapperPanel.TWrapperPanel.$init.call(this);
      this.FIsSelected$1 = false;
    };
    this.GetName$1 = function () {
      var Result = "";
      Result = this.GetName();
      return Result;
    };
    this.GetHint$1 = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("Hint",true).AttribValue;
      return Result;
    };
    this.GetmyWidth$1 = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("myWidth",true).AttribValue;
      return Result;
    };
    this.GetmyHeight$1 = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("myHeight",true).AttribValue;
      return Result;
    };
    this.GetBgColor$1 = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("BgColor",true).AttribValue;
      return Result;
    };
    this.GetAlignment$1 = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("Alignment",true).AttribValue;
      return Result;
    };
    this.SetMyName$1 = function (AValue) {
      this.SetMyName$1(AValue);
      var ob = document.getElementById(this.NodeName);
      ob.id = AValue;
      inner = document.getElementById(this.NodeName+'Contents');
      if (inner != null) {
        inner.id = AValue+'Contents';
        };
      if (this.myNode !== null) this.myNode.NodeName = AValue;
    };
    this.SetIsSelected$1 = function (AValue) {
      if (AValue !== this.FIsSelected$1) {
        this.FIsSelected$1 = AValue;
        pas.HTMLUtils.ShowHideSelectedBorder(this,this.FIsSelected$1);
      };
    };
    this.SetHint$1 = function (AValue) {
      this.myNode.SetAttributeValue$1("Hint",AValue);
      var ob = document.getElementById(this.NodeName);
      if (ob!=null)  {
      ob.title=AValue; };
    };
    this.SetAlignment$1 = function (AValue) {
      if (this.myNode !== null) {
        this.myNode.SetAttributeValue$1("Alignment",AValue);
        this.SortOutAlignment();
      };
    };
    this.SortOutAlignment = function () {
      var ParentAlignChildrenVertical = false;
      var MyAlignment = "";
      var MyLabelPos = "";
      var ParentNode = null;
      MyAlignment = this.GetAlignment$1();
      MyLabelPos = this.GetLabelPos();
      ParentNode = pas.NodeUtils.FindParentOfNode$1(pas.NodeUtils.SystemNodeTree,this.NodeName);
      ParentAlignChildrenVertical = ParentNode.FAlignChildrenVertical;
      if ((MyAlignment === "Right") || (MyAlignment === "Left")) {
        if (ParentAlignChildrenVertical === false) MyAlignment = "Top";
      } else if ((MyAlignment === "Top") || (MyAlignment === "Bottom")) {
        if (ParentAlignChildrenVertical === true) MyAlignment = "Left";
      };
      try {
             var ob = document.getElementById(this.NodeName+'Contents');
             var wrapper = document.getElementById(this.NodeName);
      
             if ((ob!=null)  && (wrapper!=null)) {
             wrapper.classList.remove('hboxNoStretch');
             wrapper.classList.remove('vboxNoStretch');
             wrapper.classList.remove('AlignmentCentre');
             wrapper.classList.remove('AlignmentRight');
             wrapper.classList.remove('AlignmentLeft');
             wrapper.classList.remove('AlignmentTop');
             wrapper.classList.remove('AlignmentBottom');
      
             if (MyAlignment=='Right') {
               if (ParentAlignChildrenVertical) {
               ob.style.float='right';
               wrapper.classList.add('AlignmentRight');
             }
             }
             else if (MyAlignment=='Left') {
             if (ParentAlignChildrenVertical) {
                 ob.style.float='left';
                 wrapper.classList.add('AlignmentLeft');
               }
               }
             else if (MyAlignment=='Centre') {
               ob.style.float='left';
                wrapper.classList.add('AlignmentCentre');
             }
      
             else if (MyAlignment=='Top') {
             if (ParentAlignChildrenVertical==false) {
               ob.style.float='left';
               wrapper.classList.add('AlignmentTop');
             }
             }
             else if (MyAlignment=='Bottom') {
             if (ParentAlignChildrenVertical==false) {
               ob.style.float='left';
               wrapper.classList.add('AlignmentBottom');
             }
           }
      
      
      
         }
       } catch(err) { alert(err.message+'  in XTabControl.SortOutAlignment'); };
    };
    this.SetMyEventTypes = function () {
      this.myEventTypes.Add("Click");
      this.myEventTypes.Add("Change");
    };
    this.Create$3 = function (MyForm, NodeName) {
      pas.WrapperPanel.TWrapperPanel.Create$2.call(this,NodeName);
      this.NodeType = $impl.MyNodeType;
      this.MyForm = MyForm;
      this.FAlignChildrenVertical = true;
      this.SetMyEventTypes();
      this.FIsContainer = true;
      this.SetBgColor$1("#FFFFFF");
      this.SetmyWidth$1("300px");
      this.SetmyHeight$1("300px");
    };
    this.SetBgColor$1 = function (AValue) {
      this.SetAttributeValue("BgColor",AValue,"Color");
      try {
      var ob = document.getElementById(this.NodeName);
      if (ob!=null) {
      ob.style.backgroundColor = AValue;  }
      } catch(err) { alert(err.message+'  in XTabControl.SetBgColor'); };
    };
    this.SetmyHeight$1 = function (AValue) {
      this.myNode.SetAttributeValue$1("myHeight",AValue);
      var ob = document.getElementById(this.NodeName);
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
    };
    this.SetmyWidth$1 = function (AValue) {
      this.myNode.SetAttributeValue$1("myWidth",AValue);
      var ob = document.getElementById(this.NodeName);
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
    };
    var $r = this.$rtti;
    $r.addProperty("IsSelected",2,rtl.boolean,"FIsSelected$1","SetIsSelected$1",{Default: false});
    $r.addProperty("Hint",3,rtl.string,"GetHint$1","SetHint$1");
    $r.addProperty("Name",3,rtl.string,"GetName$1","SetMyName$1");
    $r.addProperty("myWidth",3,rtl.string,"GetmyWidth$1","SetmyWidth$1");
    $r.addProperty("myHeight",3,rtl.string,"GetmyHeight$1","SetmyHeight$1");
    $r.addProperty("BgColor",3,rtl.string,"GetBgColor$1","SetBgColor$1");
    $r.addProperty("Alignment",3,rtl.string,"GetAlignment$1","SetAlignment$1");
  });
  rtl.createClass($mod,"TXTabSheet",pas.WrapperPanel.TWrapperPanel,function () {
    this.$init = function () {
      pas.WrapperPanel.TWrapperPanel.$init.call(this);
      this.FAlignChildrenVertical$1 = false;
      this.FIsSelected$1 = false;
    };
    this.GetName$1 = function () {
      var Result = "";
      Result = this.GetName();
      return Result;
    };
    this.GetHint$1 = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("Hint",true).AttribValue;
      return Result;
    };
    this.GetBgColor$1 = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("BgColor",true).AttribValue;
      return Result;
    };
    this.GetmyCaption = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("myCaption",true).AttribValue;
      return Result;
    };
    this.SetMyName$1 = function (AValue) {
      this.SetMyName$1(AValue);
      var ob = document.getElementById(this.NodeName);
      ob.id = AValue;
      inner = document.getElementById(this.NodeName+'Contents');
      if (inner != null) {
        inner.id = AValue+'Contents';
        };
      if (this.myNode !== null) this.myNode.NodeName = AValue;
    };
    this.SetIsSelected$1 = function (AValue) {
      if (AValue !== this.FIsSelected$1) {
        this.FIsSelected$1 = AValue;
        pas.HTMLUtils.ShowHideSelectedBorder(this,this.FIsSelected$1);
      };
    };
    this.SetHint$1 = function (AValue) {
      this.myNode.SetAttributeValue$1("Hint",AValue);
      var ob = document.getElementById(this.NodeName);
      if (ob!=null)  {
      ob.title=AValue; };
    };
    this.SetmyCaption = function (AValue) {
      this.myNode.SetAttributeValue$1("myCaption",AValue);
      ;
    };
    this.SetMyEventTypes = function () {
      this.myEventTypes.Add("Click");
    };
    this.Create$3 = function (MyForm, NodeName) {
      pas.WrapperPanel.TWrapperPanel.Create$2.call(this,NodeName);
      this.NodeType = "TXTabSheet";
      this.MyForm = MyForm;
      this.SetMyEventTypes();
      this.FIsContainer = true;
      this.SetBgColor$1("#FFFFFF");
    };
    this.SetBgColor$1 = function (AValue) {
      this.SetAttributeValue("BgColor",AValue,"Color");
      try {
      var ob = document.getElementById(this.NodeName);
      if (ob!=null) {
      ob.style.backgroundColor = AValue;  }
      } catch(err) { alert(err.message+'  in XTabControl.SetBgColor'); };
    };
    var $r = this.$rtti;
    $r.addProperty("AlignChildrenVertical",0,rtl.boolean,"FAlignChildrenVertical$1","FAlignChildrenVertical$1");
    $r.addProperty("IsSelected",2,rtl.boolean,"FIsSelected$1","SetIsSelected$1",{Default: false});
    $r.addProperty("Hint",3,rtl.string,"GetHint$1","SetHint$1");
    $r.addProperty("Name",3,rtl.string,"GetName$1","SetMyName$1");
    $r.addProperty("BgColor",3,rtl.string,"GetBgColor$1","SetBgColor$1");
    $r.addProperty("myCaption",3,rtl.string,"GetmyCaption","SetmyCaption");
  });
  $mod.$init = function () {
    pas.NodeUtils.AddNodeFuncLookup("TXTabControl",$impl.CreateTabControlInterfaceObj,$impl.CreateTabControl);
    pas.NodeUtils.AddNodeFuncLookup("TXTabSheet",$impl.CreateTabPageInterfaceObj,$impl.CreateTabSheet);
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.MyNodeType = "TXTabControl";
  $impl.CreateTabControl = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    var OnClickString = "";
    var BgColor = "";
    BgColor = MyNode.GetAttribute("BgColor",true).AttribValue;
    if (BgColor === "") BgColor = "#FFFFFF";
    OnClickString = ('onclick="event.stopPropagation();pas.Events.handleEvent(\'Click\',\'' + ScreenObjectName) + '\', this.value, \'\');" ';
    try{
        // ----------------------------------------check if the style has already been set
        var x = document.getElementsByTagName("STYLE");
        var StyleIsSet = false;
        if (x.length>0){
          for (var i=0; i<x.length; i++){
            var y= x[i].innerHTML;
            if (y.indexOf("div.TabPage") !=-1) { StyleIsSet =true}
          }
        }
    
        // ----------------------------------------add style if not already been set
        if (StyleIsSet == false)
        {
          // ----------------------------Define the styling to be used for  "TabPage"
             var Styletext='<style type="text/css">';
             Styletext=Styletext+'div.TabPage { background-color:'+BgColor+'; height:96%; width:100%}';
             Styletext=Styletext+'</style>';
    
          //----------------------------- now append the style declarations to the head of the HTML page
             document.head.innerHTML = document.head.innerHTML+Styletext;
        }
    
    
    
        var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
        wrapper.style.overflow = 'hidden';
    
        //localcontainer is an inner div.  Its id is  ScreenObjectName+'Contents'
        // It is a child of the outer container div (wrapper)
        //
         var localcontainer = document.createElement("div");
         localcontainer.id = ScreenObjectName+'Contents';
         localcontainer.style.display="inline-block;";
         localcontainer.style.height="100%";
         localcontainer.style.width="100%";
         document.getElementById(ScreenObjectName).appendChild(localcontainer);
    
      // -----------------------------Define the HTML to be used to create the Tab control
      // NB --- "TabButton" and "TabPage" are the classnames used for styling the tab controls
      // -------"TabButtonDiv" is the classname used for styling the div containing the tab buttons
    
        var TabButtonsDef = '<div id="'+ScreenObjectName+'ContentsButtons'+'" class="TabButtonDiv"'+
                            '>'+
                            '</div>';
    
      //------------------------------------ now append the declarations to the Parent
         localcontainer.innerHTML = localcontainer.innerHTML + TabButtonsDef;
    
    
        var wrapper=document.getElementById(ScreenObjectName);
        //MyNode.ScreenObject=wrapper;
      }
      catch(err) { alert(err.message+'  in XTabControl.CreateTabControl');};
    MyNode.ScreenObject = MyNode;
    MyNode.SetmyWidth$1(MyNode.GetmyWidth$1());
    MyNode.SetmyHeight$1(MyNode.GetmyHeight$1());
    MyNode.SetAlignment$1(MyNode.GetAlignment$1());
    MyNode.SetLabelPos(MyNode.GetLabelPos());
    MyNode.SetBgColor$1(MyNode.GetBgColor$1());
    MyNode.SetHint$1(MyNode.GetHint$1());
    Result = MyNode;
    return Result;
  };
  $impl.openTab = function (TabName, TabControlName) {
    var Result = "";
    try{
        var i;
     //alert('OpenTab  TabControl='+TabControlName+' TabName='+TabName);
        var x = document.getElementsByClassName(TabControlName);
        if (x==null) {alert('cannot find element by class name '+TabControlName);}
        for (i = 0; i < x.length; i++) {
           x[i].style.display = "none";
        }
        //alert('1');
       var y = document.getElementsByClassName(TabControlName+'TabButton');
       if (y==null) {alert('cannot find element by class name '+TabControlName+'TabButton');}
        for (i = 0; i <y.length; i++) {
           y[i].style.background ='#d1d0ce';// dark background when not selected
           y[i].style.border= 'none';
        }
        var selectedTab = document.getElementById(TabName);
        selectedTab.style.display = "block";
        var selectedTab = document.getElementById(TabName+'Contents');
        selectedTab.style.display = "block";
    
        var selectedTabButton = document.getElementById(TabName+'Button');
        if (selectedTabButton==null) {alert('cannot find element by name '+TabName+'Button');}
        selectedTabButton.style.background = '#f1f0ee'; // Same background color as the tab page when selected
    
        } catch(err) {alert('Error in XTabControl.OpenTab '+ err.message);};
    return Result;
  };
  $impl.CreateTabSheet = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    var ParentName = "";
    var PageCaption = "";
    var NodeID = "";
    var OnClickString = "";
    ParentName = MyNode.GetAttribute("ParentName",false).AttribValue + "Contents";
    PageCaption = MyNode.GetAttribute("myCaption",false).AttribValue;
    NodeID = MyNode.NodeName;
    OnClickString = (((((((((('onclick="event.stopPropagation();pas.XTabControl.ChangeTabPage(\'' + NodeID) + "'); ") + "pas.Events.handleEvent('Change','") + ScreenObjectName) + "','") + ScreenObjectName) + "','');") + "pas.Events.handleEvent('Click','") + NodeID) + "', '', ''); ") + '" ';
    try{
        //alert('pagecaption='+PageCaption+' parent='+ParentName);
    
        //var ParentItem = document.getElementById(ParentName);
        var ButtonsDiv = document.getElementById(ParentName+'Buttons');
    
        var buttonstring ='<button id="'+ScreenObjectName+'Button" class="'+ParentName+'TabButton" ' +
                                 OnClickString +
                              '>'+PageCaption+'</button>';
        ButtonsDiv.innerHTML = ButtonsDiv.innerHTML + buttonstring;
    
        var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,'TXTabSheet',position);
        wrapper.style.overflow = 'hidden';
        wrapper.style.height = '100%';
        wrapper.style.width = '100%';
        wrapper.className='TabPage  '+ ParentName;
    
        var TabContentDef ="<div id='" +ScreenObjectName+"Contents'  class='TabPage  "+ ParentName+"' ></div>";
        wrapper.innerHTML = wrapper.innerHTML + TabContentDef;
    
        var wrapper=document.getElementById(ScreenObjectName);
        //MyNode.ScreenObject=wrapper;
      }
      catch(err) { alert(err.message+'  in XTabControl.CreateTabSheet');};
    MyNode.ScreenObject = MyNode;
    MyNode.SetHint$1(MyNode.GetHint$1());
    Result = MyNode;
    return Result;
  };
  $impl.CreateTabControlInterfaceObj = function (MyForm, NodeName) {
    var Result = null;
    Result = $mod.TXTabControl.$create("Create$3",[MyForm,NodeName]);
    return Result;
  };
  $impl.CreateTabPageInterfaceObj = function (MyForm, NodeName) {
    var Result = null;
    Result = $mod.TXTabSheet.$create("Create$3",[MyForm,NodeName]);
    return Result;
  };
});
rtl.module("XVBox",["System","Classes","SysUtils","NodeUtils","StringUtils","WrapperPanel"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TXVBox",pas.WrapperPanel.TWrapperPanel,function () {
    this.SetMyEventTypes = function () {
      this.myEventTypes.Add("Click");
    };
    this.GetBorder = function () {
      var Result = false;
      Result = pas.StringUtils.MyStrToBool(this.myNode.GetAttribute("Border",true).AttribValue);
      return Result;
    };
    this.SetBorder = function (AValue) {
      this.myNode.SetAttributeValue("Border",pas.StringUtils.MyBoolToStr(AValue),"Boolean");
      var ob = document.getElementById(this.NodeName);
      if (ob!=null) {
      if (AValue==true) {
             ob.classList.add("normal-border");
      }
      else {
             ob.classList.remove("normal-border");
      }  };
    };
    this.Create$3 = function (MyForm, NodeName) {
      pas.WrapperPanel.TWrapperPanel.Create$2.call(this,NodeName);
      this.NodeType = $impl.MyNodeType;
      this.MyForm = MyForm;
      this.SetMyEventTypes();
      this.SetBgColor("#FFFFFF");
      this.SetmyWidth("");
      this.SetmyHeight("");
      this.SetBorder(true);
    };
    var $r = this.$rtti;
    $r.addProperty("Border",3,rtl.boolean,"GetBorder","SetBorder");
  });
  $mod.$init = function () {
    pas.NodeUtils.AddNodeFuncLookup($impl.MyNodeType,$impl.CreateinterfaceObj,$impl.CreateWidget);
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.MyNodeType = "TXVBox";
  $impl.CreateWidget = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    try{
        var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    
        var HTMLString='';
        var CentreString ='';
        var NodeIDString = "'"+ScreenObjectName+"'";
        var componentClick="'ComponentClick'";
        var MyObjectName=ScreenObjectName+'Contents';
    
    
        // for vbox containers to centre their children we mark the parent for info
        CentreString = 'class="vbox"';
        var blankParam="''";
    
        HTMLString = '<div  id="'+MyObjectName+'" '+CentreString+
                       //' style="'+Wd1+Ht1+'"'+
                       ' style="height:100%;width:100%;"'+
                       ' onclick="event.stopPropagation();pas.Events.handleEvent('+componentClick+','+NodeIDString+', this.value,'+blankParam+'); " '+
                       '></div>  ';
    
        var wrapper=document.getElementById(ScreenObjectName);
        wrapper.insertAdjacentHTML('beforeend', HTMLString);
        //MyNode.ScreenObject=wrapper;
    }catch(err) { alert(err.message+'  in XVBox.CreateVHBox');};
    MyNode.ScreenObject = MyNode;
    MyNode.SetmyHeight(MyNode.GetmyHeight());
    MyNode.SetmyWidth(MyNode.GetmyWidth());
    MyNode.SetAlignment(MyNode.GetAlignment());
    MyNode.SetHint(MyNode.GetHint());
    Result = MyNode;
    return Result;
  };
  $impl.CreateinterfaceObj = function (MyForm, Nodename) {
    var Result = null;
    Result = $mod.TXVBox.$create("Create$3",[MyForm,Nodename]);
    return Result;
  };
});
rtl.module("XMemo",["System","Classes","SysUtils","TypInfo","NodeUtils","StringUtils","WrapperPanel"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass($mod,"TXMemo",pas.WrapperPanel.TWrapperPanel,function () {
    this.SetMyEventTypes = function () {
      this.myEventTypes.Add("Click");
      this.myEventTypes.Add("Change");
    };
    this.GetItemValue = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("ItemValue",true).AttribValue;
      return Result;
    };
    this.GetReadOnly = function () {
      var Result = false;
      Result = pas.StringUtils.MyStrToBool(this.myNode.GetAttribute("ReadOnly",true).AttribValue);
      return Result;
    };
    this.GetMemoWidth = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("MemoWidth",true).AttribValue;
      return Result;
    };
    this.GetMemoHeight = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("MemoHeight",true).AttribValue;
      return Result;
    };
    this.SetItemValue = function (AValue) {
      this.myNode.SetAttributeValue$1("ItemValue",AValue);
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
         ob.value=AValue;  };
      this.LinkSaveToProperty(this);
    };
    this.SetReadOnly = function (AValue) {
      this.myNode.SetAttributeValue("ReadOnly",pas.StringUtils.MyBoolToStr(AValue),"Boolean");
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
        ob.readOnly = AValue  };
    };
    this.SetMemoWidth = function (AValue) {
      this.myNode.SetAttributeValue$1("MemoWidth",AValue);
      var ob = document.getElementById(this.NodeName+'Contents');
      //  if (ob==null) {alert(this.NodeName+'Contents'+'  not found');}
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
    };
    this.SetMemoHeight = function (AValue) {
      this.myNode.SetAttributeValue$1("MemoHeight",AValue);
      var ob = document.getElementById(this.NodeName+'Contents');
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
    };
    this.LinkLoadFromProperty = function (Sender) {
      pas.NodeUtils.TInterfaceObject.LinkLoadFromProperty.call(this,Sender);
    };
    this.LinkSaveToProperty = function (Sender) {
      if (Sender === null) ;
      if (this.FLink === null) return;
      if (this.FLink.FTIObject === null) return;
      pas.TypInfo.SetStringProp(this.FLink.FTIObject,this.FLink.FTIPropertyName,this.GetItemValue());
    };
    this.Create$3 = function (MyForm, NodeName) {
      pas.WrapperPanel.TWrapperPanel.Create$2.call(this,NodeName);
      this.NodeType = $impl.MyNodeType;
      this.MyForm = MyForm;
      this.SetMyEventTypes();
      this.FIsContainer = false;
      this.SetHint("");
      this.SetLabelText("Memo Box");
      this.SetItemValue("...text...");
      this.SetReadOnly(false);
      this.SetMemoHeight("100");
      this.SetMemoWidth("100");
    };
    var $r = this.$rtti;
    $r.addMethod("LinkLoadFromProperty",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("LinkSaveToProperty",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addProperty("ItemValue",3,rtl.string,"GetItemValue","SetItemValue");
    $r.addProperty("ReadOnly",3,rtl.boolean,"GetReadOnly","SetReadOnly");
    $r.addProperty("MemoHeight",3,rtl.string,"GetMemoHeight","SetMemoHeight");
    $r.addProperty("MemoWidth",3,rtl.string,"GetMemoWidth","SetMemoWidth");
  });
  $mod.$init = function () {
    pas.NodeUtils.AddNodeFuncLookup($impl.MyNodeType,$impl.CreateinterfaceObj,$impl.CreateWidget);
    pas.WrapperPanel.SuppressDesignerProperty("TXMemo","BgColor");
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.MyNodeType = "TXMemo";
  $impl.CreateWidget = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    var ItemValue = "";
    var LabelText = "";
    var LabelPos = "";
    var ReadOnly = false;
    var OnChangeString = "";
    var OnClickString = "";
    var OnPasteString = "";
    ItemValue = MyNode.GetAttribute("ItemValue",true).AttribValue;
    LabelText = MyNode.GetAttribute("LabelText",true).AttribValue;
    LabelPos = pas.SysUtils.UpperCase(MyNode.GetAttribute("LabelPos",true).AttribValue);
    ReadOnly = pas.SysUtils.StrToBool(MyNode.GetAttribute("ReadOnly",true).AttribValue);
    OnClickString = ('onclick="event.stopPropagation();pas.Events.handleEvent(\'Click\',\'' + ScreenObjectName) + '\', this.value,\'\');" ';
    OnChangeString = ((('onchange="pas.NodeUtils.SetInterfaceProperty(\'' + ScreenObjectName) + "','ItemValue',this.value); pas.Events.handleEvent('Change','") + ScreenObjectName) + '\', this.value, \'ItemValue\');" ';
    OnPasteString = "";
    try{
        var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
        wrapper.style.overflow = 'hidden';
    
        var HTMLString='';
        var NodeIDString = "'"+ScreenObjectName+"'";
        var componentClick="'Click'";
        var MyObjectName=ScreenObjectName+'Contents';
    
        var ReadOnlyString = '';
        if (ReadOnly==true) { ReadOnlyString = ' readonly ';}
    
        var inputtext= ItemValue;
        var Pastetypestring="'MemoPaste'";
        var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';
    
    
        var MemoString ='<textarea  id='+MyObjectName+' '+
                            OnPasteString +
                            OnClickString +
                            OnChangeString +
                       //     ' style="display: inline-block;padding:1px;height:99%;width:99%;"  cols='+numcols+' rows='+numrows+'>'+
                            ' style="display: inline-block;padding:1px;"  >'+
                           ItemValue+'</textarea> ';
    
        if (LabelPos=='LEFT') {
         HTMLString = labelstring+MemoString;
        }
        else {
         HTMLString = MemoString+labelstring;
        }
    
        var wrapper=document.getElementById(ScreenObjectName);
        wrapper.insertAdjacentHTML('beforeend', HTMLString);
       // MyNode.ScreenObject=wrapper;
      }
      catch(err) { alert(err.message+'  in XMemo.CreateXMemo');};
    MyNode.ScreenObject = MyNode;
    MyNode.SetmyHeight(MyNode.GetmyHeight());
    MyNode.SetmyWidth(MyNode.GetmyWidth());
    MyNode.SetMemoHeight(MyNode.GetMemoHeight());
    MyNode.SetMemoWidth(MyNode.GetMemoWidth());
    MyNode.SetAlignment(MyNode.GetAlignment());
    MyNode.SetLabelPos(MyNode.GetLabelPos());
    MyNode.SetLabelText(MyNode.GetLabelText());
    MyNode.SetReadOnly(MyNode.GetReadOnly());
    MyNode.SetHint(MyNode.GetHint());
    Result = MyNode;
    return Result;
  };
  $impl.CreateinterfaceObj = function (MyForm, NodeName) {
    var Result = null;
    Result = $mod.TXMemo.$create("Create$3",[MyForm,NodeName]);
    return Result;
  };
});
rtl.module("XRadioBtns",["System","Classes","SysUtils","TypInfo","NodeUtils","StringUtils","WrapperPanel"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.CreateButtonsList = function (myNode, OptionList) {
    var Result = "";
    var OnChangeString = "";
    var ItemValue = "";
    var ReadOnly = "";
    var myName = "";
    var quot = "";
    ReadOnly = myNode.GetAttribute("ReadOnly",true).AttribValue;
    ItemValue = myNode.GetAttribute("ItemValue",true).AttribValue;
    myName = myNode.NodeName;
    OnChangeString = (((('onchange="if (this.checked) {pas.NodeUtils.SetInterfaceProperty(\'' + myName) + "','ItemValue',this.value);") + "pas.Events.handleEvent('Change','") + myName) + "','";
    quot = "'";
    try{
        var ReadOnlyString = '';
        if (ReadOnly=='True') { ReadOnlyString = ' readonly ';}
    
        var HTMLString='';
        var optionlistarray=JSON.parse(OptionList);
        for (var i=0; i<optionlistarray.length; i++){
           var currentitemstring = optionlistarray[i];
           var selectedflag ='';
           if (i==ItemValue ){selectedflag = 'checked'}
           HTMLString = HTMLString +'<input type="radio"  '+selectedflag + ReadOnlyString
                                   +' id="'+myName+currentitemstring+'" '
                                   +' name='+myName+' '
                                   + OnChangeString+i+quot+');}" '
                                   +' value="'+currentitemstring+'" '
                                   +'>'+currentitemstring+'<Br>';
         }
         return HTMLString;
      }
      catch(err) { alert(err.message+'  in XRadioBtns.CreateButtonsList');};
    return Result;
  };
  rtl.createClass($mod,"TXRadioBtns",pas.WrapperPanel.TWrapperPanel,function () {
    this.SetMyEventTypes = function () {
      this.myEventTypes.Add("Click");
      this.myEventTypes.Add("Change");
    };
    this.GetReadOnly = function () {
      var Result = false;
      var tmp = "";
      tmp = this.myNode.GetAttribute("ReadOnly",true).AttribValue;
      if (tmp === "") tmp = "False";
      Result = pas.StringUtils.MyStrToBool(tmp);
      return Result;
    };
    this.GetItemValue = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("ItemValue",true).AttribValue;
      return Result;
    };
    this.GetOptionList = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("OptionList",true).AttribValue;
      return Result;
    };
    this.GetCaption = function () {
      var Result = "";
      Result = this.myNode.GetAttribute("Caption",true).AttribValue;
      return Result;
    };
    this.SetReadOnly = function (AValue) {
      this.myNode.SetAttributeValue("ReadOnly",pas.StringUtils.MyBoolToStr(AValue),"Boolean");
      //alert('setreadonly');
      var ob = document.getElementById(this.NodeName);
      if (ob!=null) {
      if (AValue==True) {ob.disabled = true}
      else {ob.disabled = false }  }
       // alert('setreadonly done');
    };
    this.SetItemValue = function (AValue) {
      this.myNode.SetAttributeValue$1("ItemValue",AValue);
      //alert('setitemvalue to '+AValue);
      var ob = document.getElementById(this.NodeName+AValue);
      if (ob!=null) {
         ob.checked=true;  }
        //alert('setitemvalue done');
      this.LinkSaveToProperty(this);
    };
    this.SetOptionList = function (AValue) {
      var myName = "";
      this.myNode.SetAttributeValue$1("OptionList",AValue);
      myName = this.NodeName;
      //alert('setoptionlist. AValue='+AValue);
      var ob = document.getElementById(myName+'Contents');
      if (ob!=null) {
        var Legend='<legend id='+myName+'ContentsLegend >"'+this.Caption+'"</legend>';
        var ItemValue=ob.value;
        var Buttons=$mod.CreateButtonsList(this.myNode,AValue);
        ob.innerHTML=Legend+Buttons;
      };
    };
    this.SetCaption = function (AValue) {
      this.myNode.SetAttributeValue$1("Caption",AValue);
      //alert('setcaption');
        var ob = document.getElementById(this.NodeName+'ContentsLegend');
        if (ob!=null) {
           ob.value=AValue  }
      //alert('setcaption done');
    };
    this.LinkLoadFromProperty = function (Sender) {
      pas.NodeUtils.TInterfaceObject.LinkLoadFromProperty.call(this,Sender);
    };
    this.LinkSaveToProperty = function (Sender) {
      if (Sender === null) ;
      if (this.FLink === null) return;
      if (this.FLink.FTIObject === null) return;
      pas.TypInfo.SetStringProp(this.FLink.FTIObject,this.FLink.FTIPropertyName,this.GetItemValue());
    };
    this.Create$3 = function (MyForm, NodeName) {
      pas.WrapperPanel.TWrapperPanel.Create$2.call(this,NodeName);
      this.NodeType = $impl.MyNodeType;
      this.MyForm = MyForm;
      this.SetMyEventTypes();
      this.FIsContainer = false;
      this.SetHint("");
      this.SetCaption("Radio Buttons");
      this.SetOptionList('["Option 1","Option 2","Option 3"]');
      this.SetItemValue("Option 1");
      this.SetReadOnly(false);
    };
    var $r = this.$rtti;
    $r.addMethod("LinkLoadFromProperty",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("LinkSaveToProperty",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addProperty("ReadOnly",3,rtl.boolean,"GetReadOnly","SetReadOnly");
    $r.addProperty("ItemValue",3,rtl.string,"GetItemValue","SetItemValue");
    $r.addProperty("Caption",3,rtl.string,"GetCaption","SetCaption");
    $r.addProperty("OptionList",3,rtl.string,"GetOptionList","SetOptionList");
  });
  $mod.$init = function () {
    pas.NodeUtils.AddNodeFuncLookup($impl.MyNodeType,$impl.CreateinterfaceObj,$impl.CreateWidget);
    pas.WrapperPanel.SuppressDesignerProperty("TXRadioBtns","LabelPos");
    pas.WrapperPanel.SuppressDesignerProperty("TXRadioBtns","LabelText");
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.MyNodeType = "TXRadioBtns";
  $impl.CreateWidget = function (MyNode, ParentNode, ScreenObjectName, position) {
    var Result = null;
    var myCaption = "";
    var OptionList = "";
    var OnClickString = "";
    myCaption = MyNode.GetAttribute("Caption",true).AttribValue;
    OptionList = MyNode.GetAttribute("OptionList",true).AttribValue;
    OnClickString = ('onclick="event.stopPropagation();pas.Events.handleEvent(\'Click\',\'' + ScreenObjectName) + '\',\'\',\'\');"';
    try{
     //   alert('create radiogroup widget');
        var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
        wrapper.style.overflow = 'hidden';
    
        var HTMLString='';
        var NodeIDString = "'"+ScreenObjectName+"'";
        var MyObjectName=ScreenObjectName+'Contents';
    
        HTMLString = '<fieldset  id='+MyObjectName+' style="display: inline-block;height:100%;width:100%;" '
                     + OnClickString
                     +' >  ';
        var Legend='<legend id='+MyObjectName+'Legend >"'+myCaption+'"</legend>';
        var Buttons=$mod.CreateButtonsList(MyNode,OptionList);
        HTMLString = HTMLString + Legend + Buttons + '</fieldset> ';
    
        var wrapper=document.getElementById(ScreenObjectName);
        wrapper.insertAdjacentHTML('beforeend', HTMLString);
        //MyNode.ScreenObject=wrapper;
      }
      catch(err) { alert(err.message+'  in XRadioBtns.CreateWidget');};
    MyNode.ScreenObject = MyNode;
    MyNode.SetmyHeight(MyNode.GetmyHeight());
    MyNode.SetmyWidth(MyNode.GetmyWidth());
    MyNode.SetAlignment(MyNode.GetAlignment());
    MyNode.SetHint(MyNode.GetHint());
    MyNode.SetItemValue(MyNode.GetItemValue());
    Result = MyNode;
    return Result;
  };
  $impl.CreateinterfaceObj = function (MyForm, NodeName) {
    var Result = null;
    Result = $mod.TXRadioBtns.$create("Create$3",[MyForm,NodeName]);
    return Result;
  };
});
rtl.module("Example1Unit",["System","Classes","SysUtils","StringUtils","NodeUtils","HTMLUtils","Popup","XScrollBox","XButton","XEditBox","XCheckBox","XHBox","XHyperLink","XTabControl","XVBox","XMemo","XRadioBtns"],function () {
  "use strict";
  var $mod = this;
  this.InitialisePage = function (dummy) {
    if (dummy === "notnow") return;
    pas.NodeUtils.StartingUp = true;
    pas.NodeUtils.loadingSystem = false;
    pas.HTMLUtils.setPasteCutAreaSize();
    $mod.Example1Form = $mod.TExample1Form.$create("Create");
    pas.NodeUtils.InitFormObject($mod.Example1Form,"Example1Form");
    $mod.Example1Form.MyRootDiv = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXScrollBox","MyRootDiv");
    $mod.Example1Form.XEditBox1 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXEditBox","XEditBox1");
    $mod.Example1Form.XCheckBox1 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXCheckBox","XCheckBox1");
    $mod.Example1Form.XHyperLink1 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXHyperlink","XHyperLink1");
    $mod.Example1Form.XHBox1 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXHBox","XHBox1");
    $mod.Example1Form.XButton2 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXButton","XButton2");
    $mod.Example1Form.XButton1 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXButton","XButton1");
    $mod.Example1Form.XTabControl1 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXTabControl","XTabControl1");
    $mod.Example1Form.XTabSheet1 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXTabSheet","XTabSheet1");
    $mod.Example1Form.XVBox1 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXVBox","XVBox1");
    $mod.Example1Form.XRadioBtns1 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXRadioBtns","XRadioBtns1");
    $mod.Example1Form.XTabSheet2 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXTabSheet","XTabSheet2");
    $mod.Example1Form.XVBox2 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXVBox","XVBox2");
    $mod.Example1Form.XRadioBtns2 = pas.NodeUtils.CreateInterfaceObject($mod.Example1Form,"TXRadioBtns","XRadioBtns2");
    pas.NodeUtils.MainForm = $mod.Example1Form;
    pas.NodeUtils.UIRootNode.MyForm = null;
    pas.NodeUtils.LoadedSystemString = '<Root|; Class |=Root|; Name |=ApplicationRoot|;ParentName|{ String|{|{True|;><Root|; Class |=Root|; Name |=UIRootNode|;ParentName|{ String|{ApplicationRoot|{True|;><Popup|; Class |=UI|; Name |=Example1Form|;ParentName|{ String|{UIRootNode|{True|;><TXScrollBox|; Class |=UI|; Name |=MyRootDiv|;Hint|{ |{|{False|;myWidth|{ |{100%|{False|;myHeight|{ |{100%|{False|;ScrollType|{ |{Both|{False|;BgColor|{ Color|{#80FFFF|{False|;Alignment|{ |{Left|{False|;ParentName|{ String|{Example1Form|{True|;><TXEditBox|; Class |=UI|; Name |=XEditBox1|;Hint|{ |{|{False|;BoxWidth|{ |{200|{False|;ItemValue|{ |{Some text to edit|{False|;ReadOnly|{ Boolean|{False|{False|;LabelText|{ |{Edit Box|{False|;LabelPos|{ |{Right|{False|;Alignment|{ |{Left|{False|;myWidth|{ |{|{False|;myHeight|{ |{|{False|;IsVisible|{ |{True|{False|;BgColor|{ Color|{|{False|;ParentName|{ String|{MyRootDiv|{True|;><\/TXEditBox><TXCheckBox|; Class |=UI|; Name |=XCheckBox1|;Hint|{ |{|{False|;myWidth|{ |{|{False|;Checked|{ Boolean|{False|{False|;ReadOnly|{ Boolean|{False|{False|;LabelText|{ |{Hide Buttons|{False|;LabelPos|{ |{Right|{False|;Alignment|{ |{Left|{False|;myHeight|{ |{|{False|;IsVisible|{ |{True|{False|;BgColor|{ Color|{|{False|;ParentName|{ String|{MyRootDiv|{True|;><\/TXCheckBox><TXHyperlink|; Class |=UI|; Name |=XHyperLink1|;Hint|{ |{|{False|;LabelCaption|{ |{BBC News|{False|;URL|{ |{http:\/\/www.bbc.co.uk\/news|{False|;Alignment|{ |{Left|{False|;myWidth|{ |{|{False|;myHeight|{ |{|{False|;LabelText|{ |{|{False|;LabelPos|{ |{|{False|;IsVisible|{ |{False|{False|;BgColor|{ Color|{|{False|;ParentName|{ String|{MyRootDiv|{True|;><\/TXHyperlink><TXHBox|; Class |=UI|; Name |=XHBox1|;Hint|{ |{|{False|;BgColor|{ Color|{#80FFFF|{False|;myWidth|{ |{|{False|;myHeight|{ |{50px|{False|;Border|{ Boolean|{True|{False|;Alignment|{ |{Left|{False|;LabelText|{ |{|{False|;LabelPos|{ |{|{False|;IsVisible|{ |{True|{False|;ParentName|{ String|{MyRootDiv|{True|;><TXButton|; Class |=UI|; Name |=XButton2|;IsVisible|{ |{True|{False|;Hint|{ |{|{False|;myWidth|{ |{130|{False|;myCaption|{ |{Show BBC Link|{False|;Enabled|{ Boolean|{True|{False|;Alignment|{ |{Centre|{False|;myHeight|{ |{|{False|;LabelText|{ |{|{False|;LabelPos|{ |{|{False|;BgColor|{ Color|{|{False|;ParentName|{ String|{XHBox1|{True|;><\/TXButton><TXButton|; Class |=UI|; Name |=XButton1|;IsVisible|{ |{True|{False|;Hint|{ |{|{False|;myWidth|{ |{130|{False|;myCaption|{ |{Disable EditBox|{False|;Enabled|{ Boolean|{True|{False|;Alignment|{ |{Centre|{False|;myHeight|{ |{|{False|;LabelText|{ |{|{False|;LabelPos|{ |{|{False|;BgColor|{ Color|{|{False|;ParentName|{ String|{XHBox1|{True|;><\/TXButton><\/TXHBox><TXTabControl|; Class |=UI|; Name |=XTabControl1|;Hint|{ |{|{False|;myWidth|{ |{600px|{False|;myHeight|{ |{300px|{False|;Alignment|{ |{Left|{False|;BgColor|{ Color|{|{False|;ParentName|{ String|{MyRootDiv|{True|;><TXTabSheet|; Class |=UI|; Name |=XTabSheet1|;Hint|{ |{|{False|;myCaption|{ |{Blue Tab|{False|;BgColor|{ Color|{#A6CAF0|{False|;ParentName|{ String|{XTabControl1|{True|;><TXVBox|; Class |=UI|; Name |=XVBox1|;Hint|{ |{|{False|;BgColor|{ Color|{#8080FF|{False|;myWidth|{ |{100%|{False|;myHeight|{ |{100%|{False|;Border|{ Boolean|{True|{False|;Alignment|{ |{Left|{False|;LabelText|{ |{|{False|;LabelPos|{ |{|{False|;IsVisible|{ |{True|{False|;ParentName|{ String|{XTabSheet1|{True|;><TXRadioBtns|; Class |=UI|; Name |=XRadioBtns1|;Hint|{ |{|{False|;Caption|{ |{Radio Buttons|{False|;OptionList|{ |{["Option 1","Option 2","Option 3"]|{False|;ItemValue|{ |{Option 1|{False|;ReadOnly|{ Boolean|{False|{False|;Alignment|{ |{Centre|{False|;myWidth|{ |{|{False|;myHeight|{ |{|{False|;LabelText|{ |{|{False|;LabelPos|{ |{|{False|;IsVisible|{ |{True|{False|;BgColor|{ Color|{|{False|;ParentName|{ String|{XVBox1|{True|;><\/TXRadioBtns><\/TXVBox><\/TXTabSheet><TXTabSheet|; Class |=UI|; Name |=XTabSheet2|;Hint|{ |{|{False|;myCaption|{ |{Yellow Tab|{False|;BgColor|{ Color|{#FFFF00|{False|;ParentName|{ String|{XTabControl1|{True|;><TXVBox|; Class |=UI|; Name |=XVBox2|;Hint|{ |{|{False|;BgColor|{ Color|{#FFFF00|{False|;myWidth|{ |{100%|{False|;myHeight|{ |{100%|{False|;Border|{ Boolean|{True|{False|;Alignment|{ |{Left|{False|;LabelText|{ |{|{False|;LabelPos|{ |{|{False|;IsVisible|{ |{True|{False|;ParentName|{ String|{XTabSheet2|{True|;><TXRadioBtns|; Class |=UI|; Name |=XRadioBtns2|;Hint|{ |{|{False|;Caption|{ |{Radio Buttons|{False|;OptionList|{ |{["Option 1","Option 2","Option 3"]|{False|;ItemValue|{ |{Option 1|{False|;ReadOnly|{ Boolean|{False|{False|;Alignment|{ |{Centre|{False|;myWidth|{ |{|{False|;myHeight|{ |{|{False|;LabelText|{ |{|{False|;LabelPos|{ |{|{False|;IsVisible|{ |{True|{False|;BgColor|{ Color|{|{False|;ParentName|{ String|{XVBox2|{True|;><\/TXRadioBtns><\/TXVBox><\/TXTabSheet><\/TXTabControl><\/TXScrollBox><\/Popup><\/Root><\/Root>';
    pas.NodeUtils.XMLToNodeTree(pas.NodeUtils.LoadedSystemString);
    pas.NodeUtils.StartingUp = false;
  };
  rtl.createClass($mod,"TExample1Form",pas.NodeUtils.TForm,function () {
    this.$init = function () {
      pas.NodeUtils.TForm.$init.call(this);
      this.MyRootDiv = null;
      this.XButton1 = null;
      this.XButton2 = null;
      this.XCheckBox1 = null;
      this.XEditBox1 = null;
      this.XHBox1 = null;
      this.XHyperLink1 = null;
      this.XRadioBtns1 = null;
      this.XRadioBtns2 = null;
      this.XTabControl1 = null;
      this.XTabSheet1 = null;
      this.XTabSheet2 = null;
      this.XVBox1 = null;
      this.XVBox2 = null;
    };
    this.$final = function () {
      this.MyRootDiv = undefined;
      this.XButton1 = undefined;
      this.XButton2 = undefined;
      this.XCheckBox1 = undefined;
      this.XEditBox1 = undefined;
      this.XHBox1 = undefined;
      this.XHyperLink1 = undefined;
      this.XRadioBtns1 = undefined;
      this.XRadioBtns2 = undefined;
      this.XTabControl1 = undefined;
      this.XTabSheet1 = undefined;
      this.XTabSheet2 = undefined;
      this.XVBox1 = undefined;
      this.XVBox2 = undefined;
      pas.NodeUtils.TForm.$final.call(this);
    };
    this.DummyPositionMarker = function () {
    };
    this.XButton1HandleButtonClick = function (nodeID, myNode, myValue) {
      if (this.XEditBox1.GetReadOnly() === true) {
        this.XEditBox1.SetReadOnly(false);
        this.XButton1.SetmyCaption("Disable EditBox");
      } else {
        this.XEditBox1.SetReadOnly(true);
        this.XButton1.SetmyCaption("Enable EditBox");
      };
    };
    this.XButton2HandleButtonClick = function (nodeID, myNode, myValue) {
      if (this.XHyperLink1.GetIsVisible()) {
        this.XHyperLink1.SetIsVisible(false);
        this.XButton2.SetmyCaption("Show BBC Link");
      } else {
        this.XHyperLink1.SetIsVisible(true);
        this.XButton2.SetmyCaption("Hide BBC Link");
      };
    };
    this.XCheckBox1HandleClick = function (nodeID, myNode, myValue) {
      if (this.XCheckBox1.GetChecked()) {
        this.XHBox1.SetIsVisible(false);
      } else {
        this.XHBox1.SetIsVisible(true);
      };
    };
    this.XRadioBtns1HandleChange = function (nodeID, myNode, myValue) {
      if (myValue === "0") {
        this.XVBox1.SetBgColor("#8080FF");
      } else if (myValue === "1") {
        this.XVBox1.SetBgColor("#6666AA")}
       else this.XVBox1.SetBgColor("#000077");
    };
    this.XRadioBtns2HandleChange = function (nodeID, myNode, myValue) {
      if (myValue === "0") {
        this.XVBox2.SetBgColor("#FFFF00");
      } else if (myValue === "1") {
        this.XVBox2.SetBgColor("#999900")}
       else this.XVBox2.SetBgColor("#444400");
    };
  });
  this.Example1Form = null;
  $mod.$init = function () {
    $mod.InitialisePage("notnow");
    try{
    // now do any Javascript specific start up code
    pas.HTMLUtils.addHandVBoxStyles();
    }catch(err) { alert(err.message+' in StartupCode');};
  };
});
