package dlua

import (
	"time"

	lua "github.com/yuin/gopher-lua"
)

//CheckTime 取得时间
func CheckTime(L *lua.LState, index int) time.Time {
	ud := L.CheckUserData(index)
	if v, ok := ud.Value.(time.Time); ok {
		return v
	}
	L.ArgError(index, "date expected")
	return time.Time{}
}

func timeNow(L *lua.LState) int {
	ud := L.NewUserData()
	ud.Value = time.Now()
	L.Push(ud)
	return 1
}

func timeSleep(L *lua.LState) int {
	microsecond := L.CheckInt(1)
	time.Sleep(time.Duration(microsecond) * time.Microsecond)
	return 0
}

func timeDate(L *lua.LState) int {
	t := time.Now()
	args := []int{t.Year(), int(t.Month()), t.Day(), t.Hour(), t.Minute(), t.Second()}
	CheckVariable(L, 1, args)
	ud := L.NewUserData()
	ud.Value = time.Date(args[0], time.Month(args[1]), args[2], args[3], args[4], args[5], 0, time.Local)
	L.Push(ud)
	return 1
}

func timeLoader(L *lua.LState) int {
	mod := L.SetFuncs(L.NewTable(), map[string]lua.LGFunction{"date": timeDate, "now": timeNow, "sleep": timeSleep})
	L.Push(mod)
	return 1
}

//timePreload 加载模块
func timePreload(L *lua.LState) {
	L.PreloadModule("time", timeLoader)
}
