> module Epic.OTTLang where

> import Epic.Language

Terms

t = x | lam x. t | t t
  | i t | hd(t) | tl(t) | 
  | switch(t) [t]
  | TY

> data OTTerm = OTRef Name -- Global or unresolved name
>             | OTV Int -- Locally bound name
>             | OTLam Name
>             | OTApp OTTerm OTTerm
>             | OTRec Tag OTTerm
>             | OTHd OTTerm
>             | OTTl OTTerm
>             | OTSwitch OTTerm [OTTerm]
>             | OTTY -- can't look at types, so dump them all here