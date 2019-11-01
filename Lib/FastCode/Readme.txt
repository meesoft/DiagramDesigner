**********************************************
********** FastCode RTL Replacement **********
**********************************************

.. What is does ..
 
  The following library will replace the current RTL
  procedure & functions used by your applications with
  faster, optimized for the latest processors.

  Currently the following functions are supported:

  CompareText, LowerCase, UpperCase, Pos, StrComp and StrCopy.
  (and more to come ...)

.. Usage ..
  
  To use it must include the "Fastcode" unit in the first order
  of your uses clauses of your delphi project. If you're using
  and alternative memory manager and/or FastMove the order should
  be like:

  FastMM4,
  FastMove,
  FastCode,
  ... etc ... 


Charalabos Michael <chmichael@creationpower.com>
