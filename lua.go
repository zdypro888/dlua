package dlua

import (
	"fmt"
	"reflect"

	luajson "github.com/alicebob/gopher-json"
	lua "github.com/yuin/gopher-lua"
	"github.com/zdypro888/workspace/done/typeinfo"
)

//RequireGlobal 添加引用
func RequireGlobal(l *lua.LState, id, modName string) error {
	if err := l.CallByParam(lua.P{
		Fn:      l.GetGlobal("require"),
		NRet:    1,
		Protect: true,
	}, lua.LString(modName)); err != nil {
		return err
	}
	mod := l.Get(-1)
	l.Pop(1)
	l.SetGlobal(id, mod)
	return nil
}

//NewState 加载
func NewState() *lua.LState {
	L := lua.NewState(lua.Options{SkipOpenLibs: true})
	for _, pair := range []struct {
		n string
		f lua.LGFunction
	}{
		{lua.LoadLibName, lua.OpenPackage},
		{lua.BaseLibName, lua.OpenBase},
		{lua.CoroutineLibName, lua.OpenCoroutine},
		{lua.TabLibName, lua.OpenTable},
		{lua.StringLibName, lua.OpenString},
		{lua.MathLibName, lua.OpenMath},
	} {
		if err := L.CallByParam(lua.P{
			Fn:      L.NewFunction(pair.f),
			NRet:    0,
			Protect: true,
		}, lua.LString(pair.n)); err != nil {
			panic(err)
		}
	}
	luajson.Preload(L)
	timePreload(L)
	return L
}

//CheckVariable 可变参数数组
func CheckVariable(L *lua.LState, index int, slice interface{}) {
	count := L.GetTop()
	length := count - index + 1
	sliceValue := reflect.ValueOf(slice)
	if sliceValue.Kind() == reflect.Ptr {
		sliceValue = sliceValue.Elem()
		sliceValue.Set(reflect.MakeSlice(sliceValue.Type(), length, length))
	} else if length > sliceValue.Len() {
		length = sliceValue.Len()
		count = length + index - 1
	}
	for i := index; i <= count; i++ {
		unmarshalReflect(L.Get(i), sliceValue.Index(i-index))
	}
}

func marshalReflect(L *lua.LState, val reflect.Value) lua.LValue {
	if !val.IsValid() {
		return lua.LNil
	}
	// Descend into pointers or interfaces
	if val.Kind() == reflect.Interface && val.NumMethod() == 0 {
		val = val.Elem()
	}
	if val.Kind() == reflect.Ptr {
		val = val.Elem()
	}
	// We got this far and still may have an invalid anything or nil ptr/interface
	if !val.IsValid() || ((val.Kind() == reflect.Ptr || val.Kind() == reflect.Interface) && val.IsNil()) {
		return lua.LNil
	}
	typ := val.Type()
	switch val.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return lua.LNumber(val.Int())
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return lua.LNumber(val.Uint())
	case reflect.Float32, reflect.Float64:
		return lua.LNumber(val.Float())
	case reflect.Bool:
		return lua.LBool(val.Bool())
	case reflect.String:
		return lua.LString(val.String())
	case reflect.Slice, reflect.Array:
		if typ.Elem().Kind() == reflect.Uint8 {
			bytes := []byte(nil)
			if val.CanAddr() {
				bytes = val.Bytes()
			} else {
				bytes = make([]byte, val.Len())
				reflect.Copy(reflect.ValueOf(bytes), val)
			}
			return lua.LString(string(bytes))
		}
		table := L.NewTable()
		for i := 0; i < val.Len(); i++ {
			table.RawSet(lua.LNumber(i+1), marshalReflect(L, val.Index(i)))
		}
		return table
	case reflect.Map:
		table := L.NewTable()
		for _, k := range val.MapKeys() {
			v := val.MapIndex(k)
			table.RawSet(marshalReflect(L, k), marshalReflect(L, v))
		}
		return table
	case reflect.Struct:
		tinfo := typeinfo.Get(typ, "lua")
		if len(tinfo.Fields) > 0 {
			table := L.NewTable()
			for _, finfo := range tinfo.Fields {
				value := finfo.Value(val)
				if !value.IsValid() || (finfo.OmitEmpty && typeinfo.IsEmpty(value)) {
					continue
				}
				table.RawSet(lua.LString(finfo.Name), marshalReflect(L, value))
			}
			return table
		}
		fallthrough
	default:
		ud := L.NewUserData()
		ud.Value = val.Interface()
		return ud
	}
}

//Marshal marshal
func Marshal(L *lua.LState, value interface{}) lua.LValue {
	return marshalReflect(L, reflect.ValueOf(value))
}

func unmarshalTableMap(table *lua.LTable) map[interface{}]interface{} {
	tm := make(map[interface{}]interface{})
	table.ForEach(func(key lua.LValue, value lua.LValue) {
		tm[unmarshalInterface(key)] = unmarshalInterface(value)
	})
	return tm
}
func unmarshalInterface(lvaue lua.LValue) interface{} {
	if lvaue.Type() == lua.LTNil {
		return nil
	}
	if lvaue.Type() == lua.LTNumber {
		return float64(lvaue.(lua.LNumber))
	}
	if lvaue.Type() == lua.LTString {
		return string(lvaue.(lua.LString))
	}
	if lvaue.Type() == lua.LTUserData {
		userData := lvaue.(*lua.LUserData)
		return userData.Value
	}
	if lvaue.Type() == lua.LTTable {
		return unmarshalTableMap(lvaue.(*lua.LTable))
	}
	return nil
}
func unmarshalSlice(table *lua.LTable, val reflect.Value) {
	val.Set(reflect.MakeSlice(val.Type(), table.Len(), table.Len()))
	for i := 1; i <= table.Len(); i++ {
		item := table.RawGet(lua.LNumber(i))
		unmarshalReflect(item, val.Index(i-1))
	}
}
func unmarshalMap(table *lua.LTable, val reflect.Value) {
	valType := val.Type()
	val.Set(reflect.MakeMap(valType))
	keyType := valType.Key()
	valueType := valType.Elem()
	table.ForEach(func(key lua.LValue, value lua.LValue) {
		vkey := reflect.New(keyType)
		unmarshalReflect(key, vkey)
		vvalue := reflect.New(valueType)
		unmarshalReflect(value, vvalue)
		val.SetMapIndex(vkey, vvalue)
	})
}
func unmarshalStruct(table *lua.LTable, val reflect.Value) {
	tinfo := typeinfo.Get(val.Type(), "lua")
	for _, field := range tinfo.Fields {
		lvalue := table.RawGet(lua.LString(field.Name))
		if lvalue != lua.LNil {
			unmarshalReflect(lvalue, field.Value(val))
		}
	}
}
func unmarshalReflect(lvalue lua.LValue, val reflect.Value) {
	if lvalue.Type() == lua.LTNil {
		val.Set(reflect.Zero(val.Type()))
		return
	}
	if val.Kind() == reflect.Ptr {
		if lvalue.Type() == lua.LTUserData {
			lud := lvalue.(*lua.LUserData).Value
			val.Set(reflect.ValueOf(lud))
			return
		}
		if val.IsNil() {
			val.Set(reflect.New(val.Type().Elem()))
		}
		val = val.Elem()
	}
	if val.Kind() == reflect.Interface && val.NumMethod() == 0 {
		v := unmarshalInterface(lvalue)
		val.Set(reflect.ValueOf(v))
		return
	}
	switch lvalue.Type() {
	case lua.LTString:
		if val.Kind() == reflect.String {
			val.SetString(string(lvalue.(lua.LString)))
		} else if val.Type().Elem().Kind() == reflect.Uint8 {
			val.SetBytes([]byte(lvalue.(lua.LString)))
		}
	case lua.LTNumber:
		switch val.Kind() {
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			val.SetInt(int64(lvalue.(lua.LNumber)))
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
			val.SetUint(uint64(lvalue.(lua.LNumber)))
		case reflect.Float32, reflect.Float64:
			val.SetFloat(float64(lvalue.(lua.LNumber)))
		}
	case lua.LTBool:
		if val.Kind() == reflect.Bool {
			val.SetBool(bool(lvalue.(lua.LBool)))
		}
	case lua.LTUserData:
		lud := lvalue.(*lua.LUserData).Value
		val.Set(reflect.ValueOf(lud))
	case lua.LTTable:
		table := lvalue.(*lua.LTable)
		if val.Kind() == reflect.Slice {
			unmarshalSlice(table, val)
		} else if val.Kind() == reflect.Struct {
			unmarshalStruct(table, val)
		} else if val.Kind() == reflect.Map {
			unmarshalMap(table, val)
		}
	}
}

//Unmarshal unmarshal
func Unmarshal(lvalue lua.LValue, value interface{}) {
	unmarshalReflect(lvalue, reflect.ValueOf(value))
}

func luaStructIndex(L *lua.LState) int {
	userData := L.CheckUserData(1)
	property := L.Get(2)
	if metatable, ok := userData.Metatable.(*lua.LTable); ok {
		metaValue := metatable.RawGet(property)
		if metaValue != lua.LNil {
			L.Push(metaValue)
			return 1
		}
	}
	target := userData.Value
	targetValue := reflect.ValueOf(target)
	if targetValue.Kind() == reflect.Ptr {
		targetValue = targetValue.Elem()
	}
	targetInfo := typeinfo.Get(targetValue.Type(), "lua")
	propertyName := string(property.(lua.LString))
	for _, field := range targetInfo.Fields {
		if field.Name == propertyName {
			L.Push(marshalReflect(L, field.Value(targetValue)))
			return 1
		}
	}
	L.ArgError(2, fmt.Sprintf("not founf property: %s", property))
	return 0
}

func luaStructNewIndex(L *lua.LState) int {
	target := L.CheckUserData(1).Value
	property := L.CheckString(2)
	lvalue := L.Get(3)
	targetValue := reflect.ValueOf(target)
	if targetValue.Kind() == reflect.Ptr {
		targetValue = targetValue.Elem()
	}
	targetInfo := typeinfo.Get(targetValue.Type(), "lua")
	for _, field := range targetInfo.Fields {
		if field.Name == property {
			unmarshalReflect(lvalue, field.Value(targetValue))
			return 0
		}
	}
	L.ArgError(2, fmt.Sprintf("not founf property: %s", property))
	return 0
}

//NewObject 新的元素
func NewObject(L *lua.LState, object interface{}, name string) (*lua.LTable, *lua.LFunction) {
	mt := L.NewTypeMetatable(name)
	L.SetField(mt, "__index", L.NewFunction(luaStructIndex))
	L.SetField(mt, "__newindex", L.NewFunction(luaStructNewIndex))

	objectType := reflect.TypeOf(object)
	if objectType.Kind() == reflect.Ptr {
		objectType = objectType.Elem()
	}
	function := L.NewFunction(func(L *lua.LState) int {
		newValue := reflect.New(objectType)
		if L.GetTop() == 1 {
			lvalue := L.Get(1)
			unmarshalReflect(lvalue, newValue)
		}
		userData := L.NewUserData()
		userData.Value = newValue.Interface()
		L.SetMetatable(userData, mt)
		L.Push(userData)
		return 1
	})
	return mt, function
}
