// Get Mission Values
local wndw is gui(300).
set wndw:x to 400. //window start position
set wndw:y to 120.
LOCAL label is wndw:ADDLABEL("Enter destruct height in km").
SET label:STYLE:ALIGN TO "CENTER".
SET label:STYLE:HSTRETCH TO True. // Fill horizontally
LOCAL destvalue is wndw:ADDTEXTFIELD("").
SET destvalue:STYLE:ALIGN TO "CENTER".
SET destvalue:STYLE:HSTRETCH TO True. // Fill horizontally
set destvalue:style:width to 300.
set destvalue:style:height to 18.
local box_Q is wndw:addhlayout().
	local Q_label is box_Q:addlabel("Max Q for staging").
	local Qvalue is box_Q:ADDTEXTFIELD("0.08"). //set to 0.2 for no sounding payload
	set Qvalue:style:width to 100.
	set Qvalue:style:height to 18.
// Show the GUI.
wndw:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
    set destvalue:onconfirm to { 
		parameter val.
		set val to val:tonumber(0).
		set destructheight to val*1000.

		set val to Qvalue:text.
		set val to val:tonumber(0).
		set sv_Q to val.

		SET isDone TO TRUE.
	}.
	WAIT 0.5.
}
wndw:HIDE().
Print "Will destruct at: " + destructheight + "m". //Range Safety height
Local sv_ClearanceHeight is 15. //tower clearance height
//Prelaunch
Wait 1. //Alow Variables to be set and Stabilise pre launch
PRINT "Prelaunch.".
Lock Throttle to 1.
SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
LOCK STEERING TO r(up:pitch,up:yaw,facing:roll).
//Liftoff
STAGE. //Ignite main engines
Print "Starting engines".
Local EngineStartTime is TIME:SECONDS.
Local MaxEngineThrust is 0. 
Local englist is List().
List Engines.
LIST ENGINES IN engList. 
FOR eng IN engList {  
	Print "eng:STAGE:" + eng:STAGE.
	Print STAGE:NUMBER.
	IF eng:STAGE >= STAGE:NUMBER { 
		SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
		Print "Stage Full Engine Thrust:" + MaxEngineThrust. 
	}
}
Print "Checking thrust ok".
Local CurrEngineThrust is 0.
Local EngineStartFalied is False.
until CurrEngineThrust > 0.99*MaxEngineThrust{ 
	Set CurrEngineThrust to 0.
	FOR eng IN engList { 
		IF eng:STAGE >= STAGE:NUMBER { 
			SET CurrEngineThrust TO CurrEngineThrust + eng:THRUST. 
		}
	}
	if (TIME:SECONDS - EngineStartTime) > 5 {
		Lock Throttle to 0.
		Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
		Print "Engine Start up Failed...Making Safe".
		Shutdown. //ends the script
	}
}
Print "Releasing Clamps".
Wait until Stage:Ready. 
STAGE. // Relase Clamps
PRINT "Lift off!!".
wait 5.
Unlock Steering. //stop wings try to provide input
Until AVAILABLETHRUST < 1{
	Wait 1.
}
Until SHIP:Q < sv_Q{
	Wait 2.
}
Stage.//Release first stage
Wait until Stage:Ready.
Stage.//Start ullage Engines
Wait until Stage:Ready.
Wait 0.5.
Stage.//Start Engines
Until ship:altitude > destructheight or ((ship:verticalspeed < 0) and (ship:altitude < 2000)){
	wait 2.	
}
wait 1.
Local P is SHIP:PARTSNAMED(core:part:Name)[0].
Local M is P:GETMODULE("ModuleRangeSafety").
M:DOEVENT("Range Safety").