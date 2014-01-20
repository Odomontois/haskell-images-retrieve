 --OOP.hs
 {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, FunctionalDependencies #-}

module OOP where 
class Provides obj iface where
    provide::obj->iface
    (#>)::obj->(iface->a)->a
    o #> meth = meth $ provide o

class Instance cls obj | obj -> cls where
    classOf::obj->cls

class Implements cls iface where
    implement::(Instance cls obj)=>cls->obj->iface

instance (Instance cls obj, Implements cls iface)=>Provides obj iface where
    provide x = implement (classOf x::cls) x

