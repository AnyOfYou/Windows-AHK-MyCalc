#NoTrayIcon
#SingleInstance,force

optr_list := "+ - * / ( ) #"
opnd_list := "0 1 2 3 4 5 6 7 8 9 ."

input = %1%
if (input!="")
{
	result := EvaluateExpression(input)
	MsgBox,260,,%1% = %result%`nCopy it?
	IfMsgBox Yes
	{
		Clipboard = %result%
		ToolTip,Copy Done
		Sleep,1000
	}
	return
}
else
{
	MsgBox,Input is Null
}
return

EvaluateExpression(str)
{
	TrimString(str)
	CStack("OPTR")
	Push("OPTR","#")
	CStack("OPND")
	rt := GetElem(str,elem)
	If(!rt)
		Return,99999999
	GetTop("OPTR",top_elem)
	while,elem != "#" || top_elem != "#"
	{
		If(rt == 1)
		{
			Push("OPND",elem)
			rt := GetElem(str,elem)
		}
		Else if(rt == 2)
		{
			GetTop("OPTR",top_elem)
			res := CompareOperator(top_elem,elem)
			If(res == -1)
			{
				PUSH("OPTR",elem)
				rt := GetElem(str,elem)
				GetTop("OPTR",top_elem)
			}
			Else If(res == 0)
			{
				Pop("OPTR",X)
				rt := GetElem(str,elem)
				GetTop("OPTR",top_elem)
			}
			Else If(res == 1)
			{
				Pop("OPTR",theta)
				Pop("OPND",b)
				Pop("OPND",a)
				res := Operate(a,theta,b)
				Push("OPND",res)
				GetTop("OPTR",top_elem)
			}
			Else
			{
				Return,99999999
			}
		}
		Else
		{
			Return,99999999
		}
	}
	GetTop("OPND",top_elem)
	Return,top_elem
}

Operate(a,optr,b)
{
	if(optr == "+")
		Return,a+b
	if(optr == "-")
		Return,a-b
	if(optr == "*")
		Return,a*b
	if(optr == "/")
		Return,a/b
}

CompareOperator(o1,o2)
{
	If(o1 == "+")
	{
		If(o2 == "+")
			Return 1
		Else if(o2 == "-")
			Return 1
		Else if(o2 == "*")
			Return -1
		Else if(o2 == "/")
			Return -1
		Else If(o2 == "(")
			Return -1
		Else if(o2 == ")")
			Return 1
		Else if(o2 == "#")
			Return 1
	}
	Else If(o1 == "-")
	{
		If(o2 == "+")
			Return 1
		Else if(o2 == "-")
			Return 1
		Else if(o2 == "*")
			Return -1
		Else if(o2 == "/")
			Return -1
		Else If(o2 == "(")
			Return -1
		Else if(o2 == ")")
			Return 1
		Else if(o2 == "#")
			Return 1
	}
	Else If(o1 == "*")
	{
		If(o2 == "+")
			Return 1
		Else if(o2 == "-")
			Return 1
		Else if(o2 == "*")
			Return 1
		Else if(o2 == "/")
			Return 1
		Else If(o2 == "(")
			Return -1
		Else if(o2 == ")")
			Return 1
		Else if(o2 == "#")
			Return 1
	}
	Else If(o1 == "/")
	{
		If(o2 == "+")
			Return 1
		Else if(o2 == "-")
			Return 1
		Else if(o2 == "*")
			Return 1
		Else if(o2 == "/")
			Return 1
		Else If(o2 == "(")
			Return -1
		Else if(o2 == ")")
			Return 1
		Else if(o2 == "#")
			Return 1
	}
	Else If(o1 == "(")
	{
		If(o2 == "+")
			Return -1
		Else if(o2 == "-")
			Return -1
		Else if(o2 == "*")
			Return -1
		Else if(o2 == "/")
			Return -1
		Else If(o2 == "(")
			Return -1
		Else if(o2 == ")")
			Return 0
		Else if(o2 == "#")
			Return 9
	}
	Else If(o1 == ")")
	{
		If(o2 == "+")
			Return 1
		Else if(o2 == "-")
			Return 1
		Else if(o2 == "*")
			Return 1
		Else if(o2 == "/")
			Return 1
		Else If(o2 == "(")
			Return 9
		Else if(o2 == ")")
			Return 1
		Else if(o2 == "#")
			Return 1
	}
	Else If(o1 == "#")
	{
		If(o2 == "+")
			Return -1
		Else if(o2 == "-")
			Return -1
		Else if(o2 == "*")
			Return -1
		Else if(o2 == "/")
			Return -1
		Else If(o2 == "(")
			Return -1
		Else if(o2 == ")")
			Return 9
		Else if(o2 == "#")
			Return 0
	}
Return 8
}

GetElem(ByRef str , ByRef elem)
{
	global optr_list,opnd_list
	ch := SubStr(str,1,1)
	If(InStr(opnd_list,ch))
	{
		GetOPND(str,elem)
		Return,1
	}
	Else if(InStr(optr_list,ch))
	{
		GetOPTR(str,elem)
		Return,2
	}
	Return,0
}

GetOPND(ByRef str,ByRef elem)
{
	global opnd_list
	len := StrLen(str)
	i := 1
	While,len--
	{
		ch := SubStr(str,i,1)
		If(!InStr(opnd_list,ch))
		{
			Break
		}
		i++
	}
	elem:= SubStr(str,1,i-1)
	str := SubStr(str,i)
}

GetOPTR(ByRef str,ByRef elem)
{
	global optr_list
	elem:= SubStr(str,1,1)
	str := SubStr(str,2)
}

TrimString(ByRef str)
{
	StringReplace,str,str,%A_Space%,,All
	StringReplace,str,str,%A_Tab%,,All
	str := str . "#"
}

CStack(stk)
{
	global
	stknum := stk . "_0"
	If (%stknum% <> "")
		Return,false
	%stknum% := 0
	Return,true
}
GetTop(stk,ByRef val)
{
	global
	local elem
	stknum := stk . "_0"
	If (%stknum% <= 0)
		Return,false
	elem := stk . "_" . %stknum%
	val := %elem%
	Return,true
}
Push(stk , val)
{
	global
	local elem
	stknum := stk . "_0"
	%stknum%++
	elem := stk . "_" . %stknum%
	%elem% := val
	Return,true
}
Pop(stk , ByRef val)
{
	global
	local elem
	stknum := stk . "_0"
	If (%stknum% <= 0)
		Return,false
	elem := stk . "_" . %stknum%
	val := %elem%
	%elem% := ""
	%stknum%--
	Return,true
}

StackEmpty(stk)
{
	stknum := stk . "_0"
	If(%stknum% == 0)
	{
		Return,true
	}
	Return,false
}