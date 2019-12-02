//Prelaunch

//Pre IGM parameters
Global f10 is 3.19840.
Global f11 is -0.544236.
Global f12 is 0.0351605.
Global f13 is -0.00116379.
Global f14 is 0.0000113886.

Global f20 is -10.9607.
Global f21 is 0.946620.
Global f22 is -0.0294206.
Global f23 is 0.000207717.
Global f24 is -0.000000439036.

Global f30 is 78.7826.
Global f31 is -2.83749.
Global f32 is 0.0289710.
Global f33 is -0.000178363.
Global f34 is 0.000000463029.

Global f40 is 69.9191.
Global f41 is -2.007490.
Global f42 is 0.0105367.
Global f43 is -0.0000233163.
Global f44 is 0.0000000136702.

Global t1 is 13.0.
Global t2 is 25.0.
Global t3 is 36.0.
Global t4 is 45.0.
Global t5 is 81.0.
Global t6 is 0.0.

Global tS1 is 35.0.
Global tS2 is 80.0.
Global tS3 is 115.0.
Global dtf is 0.

Global tAR is 160.0.//153 nominal but 160 for the tilt arrest on apollo 11

// IGM Values for boost.

Global C_ is 0.0.
Global C_O is 25.0.
Global Cf is 0.087996.
Global CosPhiL is 0.877916.
Global f0 is 32.5597.
Global f1 is -16.2615.
Global f2 is 15.6919.
Global f3 is -6.7370.
Global f4 is 26.9593.
Global f5 is -28.9526.
Global f6 is 9.8794.
Global g0 is 123.2094.
Global g1 is -56.5034.
Global g2 is -21.6675.
Global g3 is -14.5228.
Global g4 is 47.5320.
Global g5 is -22.5502.
Global g6 is 1.8946. 

Global Mdot1 is 1243.77.
Global Mdot2 is 1009.04.
Global Mdot3 is 248.882.
Global ROT = 0.
Global ROV = 1.5.

Global TTc is 4.718. // coast time
Global TT1 is 237.796. // time remaining in IGM guidance first stage
Global TT2 is 99.886. // time remaining in IGM guidance second or forth stage
Global TT1c is 342.4 // time remaining in IGM guidance boost and coast
Global TT4N is 120.565.
Global TT_3 is 120.565.
Global TT_T is 462.965.
Global t is 2.0.
Global tB1 is 50.0.
Global tB3 is 0.0.
Global dt is 1.7.
Global dtLIM is 90.0.
Global Vex1 is 4169.23.
Global Vex2 is4204.26.
Global Vex3 is 4170.57.
Global VS2T is 7007.18.
Global VTC is 300.
Global dVB is 2.0275.
Global epsilon1 is 0.0.
Global epsilon2 is 10.0.
Global epsilon3 is 10000.0.
Global epsilon4 is 8.0.
Global mu is 3986032 * (10^8).
Global tau1 is 0.
Global tau2 is 309.23.
Global tau3 is 655.86.
Global tau3N is 655.86.





TT3 // time remaining in IGM guidance third or fifth stage







Wait 1. //Alow Variables to be set and Stabilise pre launch
PRINT "Prelaunch.".
Lock Throttle to 1.
SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.


//Wait for launch window //16/07/1969 at 13:32:00 GMT 18 197 13 22 00
Set vYear to 3.
Set vDay to 60.
Set vHour to 21.
Set vMinute to 06.
Set vSecond to 30.

Until TIME:YEAR >is vYear and TIME:DAY >= vDay and Time:HOUR >= vHour and Time:MINUTE >= vMinute and Time:SECOND >= (vSecond-10) { // 
    Clearscreen.
    Print TIME:YEAR.
    Print TIME:DAY.
    Print Time:CLOCK.
    Wait 0.001.
}
Print "Mission start".
Print ship:mass.
Print ship:drymass.
// Apollo 11 72.058 azimuth heading
Set sv_intAzimith to 72.058. 
Set basetime to time:seconds + 10.
Global INU_Zero is r(up:pitch,up:yaw,facing:roll) + r(0,0,sv_intAzimith-90).
Print INU_Zero.
LOCK STEERING TO INU_Zero. 
//Ignition starts 8.9 seconds prior to lift off.
wait until Time:seconds > (basetime-8.9).   
Print "Starting engines".
Local MaxEngineThrust is 0. 
Local englist is List().
LIST ENGINES IN engList. //Get List of Engines in the vessel
//no5 -6.4
wait until Time:seconds > (basetime-6.4).   
FOR eng IN engList { 
    IF eng:TAG ="F1-C" { 
        eng:activate.
        SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
        Print "F1-C: Active".  
    }
}
//no1/3 -6.1
wait until Time:seconds > (basetime-6.1). 
FOR eng IN engList { 
    IF eng:TAG ="F1-1" or eng:TAG ="F1-3" { 
        eng:activate.
        SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
        Print "F1-1/3: Active".  
    }
}
//no4/2 -6.0/-5.9
wait until Time:seconds > (basetime-6.0). 
FOR eng IN engList { 
    IF eng:TAG ="F1-4" or eng:TAG ="F1-2" { 
        eng:activate.
        SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
        Print "F1-2/4: Active".  
    }
}
STAGE. // Engine stage as activate doesn't stage
Print "Checking thrust ok".
Local CurrEngineThrust is 0.
Local EngineStartFalied is False.
Local EngineStartTime is TIME:SECONDS.
until CurrEngineThrust > 0.97*MaxEngineThrust{ 
    Set CurrEngineThrust to 0.
    FOR eng IN engList { 
        IF eng:IGNITION ="true"{ 
            SET CurrEngineThrust TO CurrEngineThrust + eng:THRUST. 
        }
    }
    //Print CurrEngineThrust.
    
    if Time:seconds > (basetime-0.0) {
        Print time:seconds.
        Lock Throttle to 0.
        Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
        Print "Engine Start up Failed...Making Safe".
        Shutdown. //ends the script
    }
}
//all thrust ok -1.6
Print "Thrust ok: " + (basetime - time:seconds).
Print time:seconds.
LOCK STEERING TO INU_Zero. //
wait until Time:HOUR >= vHour and Time:MINUTE >= vMinute and Time:SECOND >= vSecond.
Set liftoff to time:seconds.
Print "T0: " + liftoff.
Print ship:mass.
Print ship:drymass.
//0.3 first lift off arms released
wait until Time:seconds > (liftoff + 0.3). 
STAGE. // Relase Clamps
Print "Lift off!! " + (time:seconds - liftoff).
//0.6 umbilical disconnect
wait until Time:seconds > (liftoff + 0.6). 
Print "Umbilical disconect " + (time:seconds - liftoff).
Local dTTf is 1.
Local I is 1.
Local Majorloop is 0.
UNTIL time:seconds > (liftoff + tAR){
    Set tc to (time:seconds - liftoff).
	Set SteeringManager:MAXSTOPPINGTIME to 0.2.
	If (tc - Majorloop) > dTTf {
    	f_Pre_IGM(tc, sv_intAzimith).
		Set Majorloop to tc.
		//Print "Major loop:" + (tc - Majorloop).
	}
	// Clear tower yaw manuever // 1.7
	If (time:seconds > (liftoff + 1.7)) and (I = 1){
	//LOCK STEERING TO HEADING(90, (88.75)).//1.25 degree yaw at 1 degree per second and ramped back to 0 slowly
		Print "Yaw program start" + (time:seconds - liftoff).
		Set I to I+1.
	}
	// end yaw program 9.7
	If (time:seconds > (liftoff + 9.7)) and (I = 2){
	// LOCK STEERING TO HEADING(90, 88.75).
		Print "Yaw program end " + (time:seconds - liftoff).
		Set I to I+1.
	}
	// //Pitch and roll program commences at 13.2 seconds
	If (time:seconds > (liftoff + 13.2)) and (I = 3){
	// LOCK STEERING TO HEADING(sv_intAzimith, 88.75).
		SET STEERINGMANAGER:MAXSTOPPINGTIME TO 0.9.//0.4
		Print "Pitch and roll program " + (time:seconds - liftoff).
		Set I to I+1.
	}
	// ourboard engine CANT 20.6
	// end roll program 31.1
	If (time:seconds > (liftoff + 31.1)) and (I = 4){
		Print "End roll program " + (time:seconds - liftoff).
	// lock pitch to 90 - VANG(SHIP:UP:VECTOR, SHIP:VELOCITY:SURFACE).
	// LOCK STEERING TO heading(sv_intAzimith, pitch).
		SET STEERINGMANAGER:MAXSTOPPINGTIME TO 5.
		Set I to I+1.
	}
	// mach 1 66.3
	If (time:seconds > (liftoff + 66.3)) and (I = 5){
		Print "Mach 1 " + (time:seconds - liftoff).
	 	Print "Speed: " + SHIP:AIRSPEED.
	 	Print "Altitude: " + altitude.
		Set I to I+1.
	}
	// Max Q at 83
	If (time:seconds > (liftoff + 83)) and (I = 6){
		Print "Max Q " + (time:seconds - liftoff).
		Print "Max Q: " + SHIP:Q.
		Print "Speed: " + SHIP:AIRSPEED.
		Print "Altitude: " + altitude.
		Print ship:mass.
		Print ship:drymass.
		Set I to I+1.
	}
	//inboard engine cutoff 135.2
	If (time:seconds > (liftoff + 135.2)) and (I = 7){
		FOR eng IN engList { 
			IF eng:TAG ="F1-C" { 
				eng:shutdown.
			}
		}
		Print "Centre shutdown " + (time:seconds - liftoff).
		Print "Speed: " + SHIP:AIRSPEED.
		Print "Altitude: " + altitude.
		Print ship:mass.
		Print ship:drymass.
		Set I to I+1.
	}
	wait 0.0001.
}//End Pre-IGM phase

//tilt arrest 160 occurs at the end of the loop
//Wait UNTIL (time:seconds > (liftoff + 160)).
//Set sV to ship:facing:forevector.
//lock steering to lookdirup( sV, ship:facing:topvector ).
Print "Tilt Arrest Enabled "+ (time:seconds - liftoff).

//MECO Staging 161.63
Wait UNTIL (time:seconds > (liftoff + 161.63)).
FOR eng IN engList { 
	IF eng:IGNITION ="true"{ 
		eng:shutdown. 
	}
}
Print "MECO:"+(TIME:SECONDS).
Print "Speed: " + SHIP:AIRSPEED.
Print "Altitude: " + altitude.
Print ship:mass.
Print ship:drymass.

//162.1 Ullage motors
Wait UNTIL time:seconds > (liftoff + 162.1).
Stage.
//162.3 retro motor fire (already build into MECO satge)
Wait UNTIL time:seconds > (liftoff + 162.3).
wait until stage:ready.
Stage. // release S-IC
//163.0 S-II engine start
Wait UNTIL time:seconds > (liftoff + 163.0).
wait until stage:ready.
Stage.
Print "Second Stage Ignition".
Print ship:mass.
Print ship:drymass.
//192.3 seperate aft interstage
Wait UNTIL time:seconds > (liftoff + 192.3).
Stage.
Print "S-II aft interstage release".
Print ship:mass.
Print ship:drymass.
//197.9 LES release
Wait UNTIL time:seconds > (liftoff + 197.9).
Stage.
Print "S-LES release".
Print ship:mass.
Print ship:drymass.
//204.1 IGM phase 1 (Iterative guidance mode)
f_IGM_Steer().




//460.6 Centre engine cutout
Local englist is List().
LIST ENGINES IN engList.
Wait UNTIL time:seconds > (liftoff + 460.6).
FOR eng IN engList { 
    IF eng:TAG ="J2-C" { 
        eng:shutdown.
    }
}
Print "Centre shutdown " + (time:seconds - liftoff).
Print "Speed: " + SHIP:AIRSPEED.
Print "Altitude: " + altitude.
Print ship:mass.
Print ship:drymass.
// 494.4 IGM phase 2 and tau mode

// 504.2 end tau mode

// 548.22 engine cutout and IGM phase 3
Wait UNTIL time:seconds > (liftoff + 548.22).
FOR eng IN engList { 
    IF eng:IGNITION ="true"{ 
        eng:shutdown. 
    }
}
Print "ECO:"+(TIME:SECONDS).
Print "Speed: " + SHIP:AIRSPEED.
Print "Altitude: " + altitude.
Print ship:mass.
Print ship:drymass.
Stage.
// 548.9 ullage motors
Wait UNTIL time:seconds > (liftoff + 548.9).
wait until stage:ready.
Stage.
//549.0 seperation
Wait UNTIL time:seconds > (liftoff + 549.0).
wait until stage:ready.
Stage.
//549.2 engine start
Wait UNTIL time:seconds > (liftoff + 549.2).
wait until stage:ready.
Stage.
//555.6 tau mode

//561.0 ullage case jettison
//Wait UNTIL time:seconds > (liftoff + 561.0).
//Stage.
//562.4 end of tau mode

//665.2 begin terminal guidance

//691.6 end IGM phase 3

//699.34 ECO command
Wait UNTIL time:seconds > (liftoff + 699.34).
FOR eng IN engList { 
    IF eng:TAG ="J2-1" { 
        eng:shutdown.
    }
}
Print "ECO:"+(TIME:SECONDS).
Print "Speed: " + SHIP:AIRSPEED.
Print "Altitude: " + altitude.
Print ship:mass.
Print ship:drymass.
//699.8 APS ullage command

//709.34 Orbit instertion

//719.3 horitzontal move to

//786.5 ullage engine turn off

wait 2.
Print "Stage Finshed".
Shutdown. //ends the script

function f_Pre_IGM {
Parameter tc, Az.
    Local Xy is 0.
    Local Xz is 0.
    Local Xx is 0.
	Print tc.

    If (tc < t1) or (alt:radar < 137){
			f_Yaw_Man(Tc).
			Return.//end function
	}

	If (tc <i= t2){
		If (tc <= t6){
			//zero time locked at launch with engine out
		}
	}
	If (tc <= tAR){ f_Pre_IGM_Steer(tc).}
}

function f_Yaw_Man{
Parameter tc.
    if 1.0 > tc { LOCK STEERING to INU_Zero + r(0, 0, 90-sv_intAzimith). }.
    if (1.0 <= Tc) and (tc < 8.75) { LOCK STEERING to (INU_Zero + r(1.25, 0, 90-sv_intAzimith)). }.
    if 8.75 <=tc { LOCK STEERING to INU_Zero + r(0, 0, 90-sv_intAzimith). }.
}

function f_Pre_IGM_Steer{
Parameter tc.
	local Xy is 0.
	//If dtf is 0 { Set dtf to tc.}//fix up for engine out freeze.
	Local dtcf is (tc-dtf).//tc is time from liftoff dtf is period of frozen Xy 

    If dTcf < ts1{ Set Xy to f_Pitch_Man(f10, f11, f12, f13, f14, dtcf).}
    If (ts1 <= dtcf) and (dtcf < ts2){ Set Xy to f_Pitch_Man(f20, f21, f22, f23, f24, dtcf).}
    If (ts2 <= dtcf) and (dtcf < ts3){ Set Xy to f_Pitch_Man(f30, f31, f32, f33, f34, dtcf).}
    If ts3 <= dtcf { Set Xy to f_Pitch_Man(f40, f41, f42, f43, f44, dtcf).}
	//LOCK STEERING to INU_Zero + r(0, Xy, 0).
	LOCK STEERING to Heading(sv_intAzimith, 90 + Xy).
	Print "IGM: " + Xy.
	Print "dtf:" + dtf.
	Print "dtcf:" + dtcf.
}

function f_pitch_Man {
Parameter val0, val1, val2, val3, val4, dtcf.
    Return val0 + (val1*dtcf) + (val2*(dtcf^2))+(val3*(dtcf^3))+ (val4*(dtcf^4)).
}

function f_IGM_Steer{

If TT1 > 0 {// check if time remaining in IGM guidance first stage
	tau1 is Vex1(ship:mass/ship:thrust).
	f_CHI_Tilde
}else{
	tau2 is tau2 + TT1 * (Mdot1/Mdot2)
	if T2 > 0 {
		TT2 is TT2 +TT1 * (Mdot1/Mdot2)
		TT1 is 0
		f_CHI_Tilde
	}Else{
		TT2 is 0
		TT1 is 0
		f_CHI_Tilde
	}
}


//Logic
TT1 <0 -> 




}

function f_CHI_Tilde{

// stage integral calcs
	f_Stage_int_calcs().
	if TT_T < epsilon1 {

	}




Global C_ is 0.0.
Global C_O is 25.0.
Global Cf is 0.087996.
Global CosPhiL is 0.877916.
Global f0 is 32.5597.
Global f1 is -16.2615.
Global f2 is 15.6919.
Global f3 is -6.7370.
Global f4 is 26.9593.
Global f5 is -28.9526.
Global f6 is 9.8794.
Global g0 is 123.2094.
Global g1 is -56.5034.
Global g2 is -21.6675.
Global g3 is -14.5228.
Global g4 is 47.5320.
Global g5 is -22.5502.
Global g6 is 1.8946. 

Global Mdot1 is 1243.77.
Global Mdot2 is 1009.04.
Global Mdot3 is 248.882.

Global TTc is 4.718. // coast time
Global TT1 is 237.796. // time remaining in IGM guidance first stage
Global TT2 is 99.886. // time remaining in IGM guidance second or forth stage
Global TT1c is 342.4 // time remaining in IGM guidance boost and coast
Global TT4N is 120.565.
Global TT_3 is 120.565.
Global TT_T is 462.965.
Global t is 2.0.
Global tB1 is 50.0.
Global tB3 is 0.0.
Global dt is 1.7.
Global dtLIM is 90.0.
Global Vex1 is 4169.23.
Global Vex2 is 4204.26.
Global Vex3 is 4170.57.
Global VS2T is 7007.18.
Global VTC is 300.
Global dVB is 2.0275.
Global epsilon1 is 0.0.
Global epsilon2 is 10.0.
Global epsilon3 is 10000.0.
Global epsilon4 is 8.0.
Global mu is 3986032 * (10^8).
Global tau1 is 0.
Global tau2 is 309.23.
Global tau3 is 655.86.
Global tau3N is 655.86.


}

function f_Stage_int_calcs{
//Calcs

Global L1 is Vex1 LN(tau1/(tau1-TT1)).
Global J1 is (L1*tau1) - (Vex1*TT1).
Global S1 is (L1*TT1) - J1.
Global Q1 is (S1*tau1)-(Vex1*((TT1^2)/2)
Global P1 is (J1*tau1)-(Vex1*((TT1^2)/2)
Global U1 is (Q1*tau1)-(Vex1*((TT1^3)/6)
Global L2 is Vex2 LN(tau2/(tau2-TT2)).
Global J2 is (L2*tau2) - (Vex2*TT2).
Global S2 is (L2*TT2) - J2.
Global Q2 is (S2*tau2)-(Vex2*((TT2^2)/2).
Global P2 is (J2*tau2)-(Vex2*((TT2^2)/2).
Global U2 is (Q2*tau2)-(Vex2*((TT2^3)/6).
Global L12 is L1 + L2.
Global J12 is J1 + J2 + (L2*TT1).
Global S12	is S1 - J2 + (L12*(TT2 + TTc)).
Global Q12 is Q1 + Q2 + (S1*TT1) + (J1*TT2).
Global P12 is P1 + P2 + (TT1*((2*J2) + (L2*TT1)).
Global U12 is U1 + U2+ (TT1*((2*Q2) + (S2*TT1)) + (TT2*P1).
Global L_3 is Vex3 * LN(tau3/(tau3-TT_3)).
Global J_3 is (L_3*tau3) - (Vex3*TT_3).
Global L_y is L12 + L_3.
}
function f_Range1_calcs{
//Range angle 1

delta2

phiT
}

function f_Range2_calcs{
//Range angle 2

V

R

sinGamma

CosGamma

Phidot1
PhiDotT
PhiT

}