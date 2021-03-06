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

Wait 1. //Alow Variables to be set and Stabilise pre launch
PRINT "Prelaunch.".
Lock Throttle to 1.
SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.

//Wait for launch window //16/07/1969 at 13:32:00 GMT 18 197 13 22 00
Set vYear to 5.
Set vDay to 340.
Set vHour to 00.
Set vMinute to 12.
Set vSecond to 50.

Until TIME:YEAR >= vYear and TIME:DAY >= vDay and Time:HOUR >= vHour and Time:MINUTE >= vMinute and Time:SECOND >= (vSecond-17) { // 
    Clearscreen.
    Print TIME:YEAR.
    Print TIME:DAY.
    Print Time:CLOCK.
    Wait 0.001.
}
Print "Mission start".
Print ship:mass.
Print ship:drymass.
//Print Body:ATM:SEALEVELPRESSURE.
//Print Body:ATM:ALTITUDEPRESSURE(1230).
// Apollo 11 72.058 azimuth heading
Set sv_intAzimith to 72.058. 
Set basetime to time:seconds + 17.
Global INU_Zero is r(up:pitch,up:yaw,facing:roll) + r(0,0,sv_intAzimith-90).
Print INU_Zero.
LOCK STEERING TO INU_Zero. 
//Ignition starts 8.9 seconds prior to lift off.
wait until Time:seconds > (basetime-8.9).   
Print "Starting engines".
Local MaxEngineThrust is 0. 
Local englist is List().
Local RO_Engine_offset is 1.5. // 5.5 to allow for RO engine startup delay compared to real engines

LIST ENGINES IN engList. //Get List of Engines in the vessel
//no5 -6.4
wait until Time:seconds > (basetime-(6.4+RO_Engine_offset)).   
FOR eng IN engList { 
    IF eng:TAG ="F1-C" { 
        eng:activate.
        SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
        Print "F1-C: Active".  
    }
}
//no1/3 -6.1
wait until Time:seconds > (basetime-(6.1+RO_Engine_offset)). 
FOR eng IN engList { 
    IF eng:TAG ="F1-1" or eng:TAG ="F1-3" { 
        eng:activate.
        SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
        Print "F1-1/3: Active".  
    }
}
//no4/2 -6.0/-5.9
wait until Time:seconds > (basetime-(6.0+RO_Engine_offset)). 
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
Print ship:mass.
Print ship:drymass.
//0.6 umbilical disconnect
wait until Time:seconds > (liftoff + 0.6). 
Print "Umbilical disconect " + (time:seconds - liftoff).
Local dTTf is 1.
Local I is 1.
Local Majorloop is 0.
UNTIL time:seconds > (liftoff + tAR){
    Set tc to (time:seconds - liftoff).
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
		Global INU_Zero_ is INU_Zero + r(sv_intAzimith-90,0,0).
	}
	// //Pitch and roll program commences at 13.2 seconds
	If (time:seconds > (liftoff + 13.2)) and (I = 3){
	// LOCK STEERING TO HEADING(sv_intAzimith, 88.75).
		Print "Pitch and roll program " + (time:seconds - liftoff).
		Set I to I+1.
	}
	// ourboard engine CANT 20.6
	// end roll program 31.1
	If (time:seconds > (liftoff + 31.1)) and (I = 4){
		Print "End roll program " + (time:seconds - liftoff).
	// lock pitch to 90 - VANG(SHIP:UP:VECTOR, SHIP:VELOCITY:SURFACE).
	// LOCK STEERING TO heading(sv_intAzimith, pitch).
		SET STEERINGMANAGER:MAXSTOPPINGTIME TO 1.
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
//197.9 LET release
Wait UNTIL time:seconds > (liftoff + 197.9).
Stage.
Print "S-LET release".
Print ship:mass.
Print ship:drymass.
//204.1 IGM phase 1 (Iterative guidance mode)
f_IGM_Steer(459, 191300, 191300, 31.381, 548, (5140300/(286900)), (901220/(166300)), 4170, 248, 166300 ).
SET STEERINGMANAGER:MAXSTOPPINGTIME TO 0.1.

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

f_IGM_Steer(497, 191300, 191300, 31.381, 548, (5140300/(286900)), (901220/(166300)), 4170, 248, 166300 ).
// 494.4 IGM phase 2 and tau mode
//f_IGM_Steer(49,191300, 191300, 31.381, 548, (3116000/(286900)), (901220/(166300)), 4170, 2148, 166300 ).

// end looping time so go back to main function
// target periapsis
// target apoapsis
//target inclination
//stage 1 time when fuel burnt out.
// the estimate of the ships acceleration at the stage end
// s2_acc_start.
// s_ve2.
// mass_flow.
// start_mass.
// u is 0. // target true anomaly in degrees(0 = insertion at Pe)

//498 High(5.5) to low MRS(4.34) command 

// "change to closed loop"...changeEMRmode
Wait UNTIL time:seconds > (liftoff + 498).
LIST ENGINES IN engList.
FOR eng IN engList { 
    IF eng:TAG ="J-2" { 
        Local M is eng:GETMODULE("EMRController").
		M:DOAction("change EMR mode", true).
    }
}


f_IGM_Steer(547,191300, 191300, 31.381, 548, (3116000/(286900)), (901220/(166300)), 4170, 248, 166300 ).

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
RCS on.
Wait UNTIL time:seconds > (liftoff + 699.34).
FOR eng IN engList { 
    IF eng:name ="FASAApolloAPS" { 
        eng:shutdown.
    }
}
//555.6 tau mode

//561.0 ullage case jettison
//Wait UNTIL time:seconds > (liftoff + 561.0).
//Stage.
//562.4 end of tau mode
f_IGM_Steer_Orbit(691,191300,191300,31.381).

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
Print ship:wetmass.
Print ship:drymass.
//699.8 APS ullage command
FOR eng IN engList { 
    IF eng:name ="FASAApolloAPS" { 
        eng:activate.
    }
}
//709.34 Orbit instertion

//719.3 horitzontal move to
LOCK STEERING TO heading(90, 0).
//786.5 ullage engine turn off
Wait until ship:orbit:eccentricity < 0.01.
wait 2.
Print "Stage Finshed".
Shutdown. //ends the script

function f_Pre_IGM {
Parameter tc, Az.
    Local Xy is 0.
    Local Xz is 0.
    Local Xx is 0.
	//Print tc.

    If (tc < t1) or (alt:radar < 137){
			f_Yaw_Man(Tc).
			Return.//end function
	}

	If (tc <= t2){
		If (tc <= t6){
			//zero time locked at launch with engine out
		}
	}
	If (tc <= tAR){ f_Pre_IGM_Steer(tc).}
}

function f_Yaw_Man{
Parameter tc.
    if 1.0 > tc { LOCK STEERING to INU_Zero + r(0, 0, 90-sv_intAzimith). }.
    if (1.0 <= Tc) and (tc < 8.75) { 
		LOCK STEERING to (INU_Zero + r(1.25, 0, 90-sv_intAzimith)). 
		Set SteeringManager:MAXSTOPPINGTIME to 0.2.	
	}.
    if 8.75 <=tc { 
		LOCK STEERING to INU_Zero. 
		Set SteeringManager:MAXSTOPPINGTIME to 1.
	}.
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
	//Print "IGM: " + Xy.
	//Print "dtf:" + dtf.
	//Print "dtcf:" + dtcf.
}

function f_pitch_Man {
Parameter val0, val1, val2, val3, val4, dtcf.
    Return val0 + (val1*dtcf) + (val2*(dtcf^2))+(val3*(dtcf^3))+ (val4*(dtcf^4)).
}

function f_IGM_Steer{
//Used for second stage taking into account the third "end" stage
/////////////////////////////////////////////////////////////////////////////////////
// Credits: Own modifications to:
// http://www.orbiterwiki.org/wiki/Powered_Explicit_Guidance
//With Large assisstance and corrections from:
// https://github.com/Noiredd/PEGAS
// https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19660006073.pdf
// https://amyparent.com/post/automating-rocket-launches/

Parameter End. //end looping time so go back to main faunction
Parameter tgt_pe. //target periapsis
Parameter tgt_ap. //target apoapsis
Parameter tgt_inc. //target inclination
Parameter Stage_End.//stage 1 time when fuel burnt out.
Parameter s_acc_end. // the estimate of the ships acceleration at the stage end
parameter s2_acc_start.
parameter s_ve2.
parameter mass_flow.
parameter start_mass.
parameter u is 0. // target true anomaly in degrees(0 = insertion at Pe)

    Local tc to (time:seconds - liftoff). // current mission time progression since lift off
    local last is tc. //starting mission elased time at last point of calculation
    local A1 is 0. //starting peg variable
    local B1 is 0. //starting peg variable
    local C1 is 0. //starting peg variable

    local A2 is 0. //starting peg variable
    local B2 is 0. //starting peg variable
    local C2 is 0. //starting peg variable

    local converged is -3.
    local delta is 0. //time between peg loops
	local T1 is Stage_end - Tc. //intial guess on time to thrust cut off
	local peg_step is 1.0.//time between each calcuation check
    local s_r is ship:orbit:body:distance. // current ship parameter
    local s_acc is ship:AVAILABLETHRUST/ship:mass. // current ship parameter
	Print "s_acc " + s_acc.
    local s_vy is ship:verticalspeed. // current ship parameter
    local s_vx is sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2). // current ship parameter
	local s_ve is f_Vel_Exhaust().
	local w is s_vx / s_r.
	Set tau to s_ve/s_acc. //time to burn ship if all propellant
	//values setup
    set ra to body:radius + tgt_ap. //full Ap
    set rp to body:radius + tgt_pe. //full pe
    local sma is (ra+rp)/2. //sma
    local ecc is (ra-rp)/(ra+rp). //eccentricity
    local vp is sqrt((2*body:mu*ra)/(rp*2*sma)). // this is the target velocity at the periapsis
	Print "vp " +vp. 
    local rc is (sma*(1-ecc^2))/(1+ecc*cos(u)). // this is the target radius based on the desire true anomoly
    print "rc "+rc.
    local vc is sqrt((vp^2) + 2*body:mu*((1/rc)-(1/rp))). // this is the target velocity at the target radius (if u is zero this will equal vp)
    print "vc "+vc.
    local uc is 90 - arcsin((rp*vp)/(rc*vc)). // this is the direction vector of the target velocity
    // Define target position and velocities
    local tgt_r is rc.
    local tgt_vy is vc*sin(uc). // this is the split of the target velocity at the point in time
    local tgt_vx is vc*cos(uc). // this is the split of the target velocity at the point in time (should be zero for u = 0)
    Print "tgt_vx " + tgt_vx.
	Print "tgt_vy " + tgt_vy.
    Local tgt_h is vcrs(v(tgt_r, 0, 0), v(tgt_vy, tgt_vx, 0)):mag. // target angular momentum
	
    local peg is hf_PEG_ABT(A1, B1, T1, peg_step, tau, tgt_vy, tgt_vx, tgt_r, s_vy, s_vx, s_r, s_acc_end).  // inital run through the cycle with first estimations
    wait 0.001.
	Set A1 to peg[0].
	Set B1 to peg[1].
	Set T1 to Stage_end - Tc. // fix peg[3].

	local s_pitch1 is 0.
	//Solve for w at stage 1 end using current A and B values
	//solve for T2 using current w value
	//iterate solving stage 1 A and B values using the w at stage w value obtained.


	//iterative calculate stage ending w
	local tgt_w is (s_vy/s_r)*1.1. //first guess at tgt_w.
	local peg_w is list().
	set peg_w to hf_PEG_w (A1, B1, T1, s_acc_end, tgt_w).
	Set tgt_w to peg_w[0].
	Print tgt_w.
	Local r_w is peg_w[1].
	local rdot_w is peg_w[2].
	local hT1 is peg_w[3].
	local dAdB is hf_PEG_dA_dB (tgt_w, r_w, rdot_w, T1, s_acc_end, s2_acc_start, s_ve2 ).
	Local T2 is 100. //intial estimate
	Local dA is dAdB[0].
	Local dB is dAdB[1].
	set T2_new to hf_PEG_T2( dA, dB, hT1, r_w, tgt_h, T1, s_acc_end, s2_acc_start, s_ve2, tgt_w, mass_flow, start_mass, T2, A1, B1, tgt_r).

	set T2 to T2_new[0].
	Set A2 to T2_new[1].
	Set B2 to T2_new[2].
	Set C2 to T2_new[3].

	Print "T2: " +T2.
	Print r_w.
	Print dA.
	Print dB.
    Print "Entering Convergence loop".
	//Loop through updating the parameters until the break condition is met
    until false {
		// collect current ship parameter
        Set tc to (time:seconds - liftoff).
        set s_r to ship:orbit:body:distance.
		set s_acc to ship:AVAILABLETHRUST/ship:mass.
		set s_vy to ship:verticalspeed.
		set s_vx to sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
		Set w to s_vx / s_r.
		Set tau to s_ve/s_acc.
        set delta to tc - last. // time since last calculation
		Print "Mission Time " + tc.
		Print "delta " + delta.
		Print "peg_step " + peg_step.

        If (delta >= peg_step) {  // this is used to ensure a minimum time step occurs before undertaking the next peg cycle
            //f_peg_vis().
			Print "Convergence Step#####################################################".
			//estimate A1, B1, T1.
			Set peg to hf_PEG_ABT(A1, B1, T1, peg_step, tau, tgt_vy, tgt_vx, tgt_r, s_vy, s_vx, s_r, s_acc_end).
			
			set A1 to peg[0].
            set B1 to peg[1].
			set C1 to peg[2].
			Set T1_new to Stage_end - Tc. // fixpeg[3].

			//iterative calculate stage ending w
			set peg_w to hf_PEG_w (A1, B1, T1_new, s_acc_end, tgt_w).

			Set tgt_w to peg_w[0].
			Set r_w to peg_w[1].
			Set rdot_w to peg_w[2].
			Set hT1 to peg_w[3].

			Set dAdB to hf_PEG_dA_dB (tgt_w, r_w, rdot_w, T1_new, s_acc_end, s2_acc_start, s_ve2 ).

			Set dA to dAdB[0].
			Set dB to dAdB[1].

			Print "Tgt w: " + tgt_w.
			Print "r_w: " + r_w.
			Print "vo: " + tgt_w * r_w.

			set T2_new to hf_PEG_T2( dA, dB, hT1, r_w, tgt_h, T1_new, s_acc_end, s2_acc_start, s_ve2, tgt_w, mass_flow, start_mass, T2, A1, B1, tgt_r).

			set T2 to T2_new[0].
			Set A2 to T2_new[1].
			Set B2 to T2_new[2].
			Set C2 to T2_new[3].

			Print "T2: " +T2.
			Set last to (time:seconds - liftoff).

			Print "Conv Step percent: " + abs( (T1-2*peg_step)/T1_new-1 ).
			Print T1.
			Print T1_new.
			Print delta.
			if abs( (T1-2*peg_step)/T1_new-1 ) < 0.05 {  //if the time returned is within 5% of the old T guess to burnout allow convergence to progress 
				//ClearScreen.
                if converged < 0 {
                    set converged to converged+1. //(this is done over 3 cycles to ensure the convergence solution selected is accurate enough over ten ship location updates rather than relying on only one convergence solution to enter a closed loop)
					Print "Convergence step +1".
                } else if converged = 0 {
                    set converged to 1.
                    Print("closed loop enabled").
                }
            } 
			set T1 to T1_new.

			Set delta to 0.
			
			set s_pitch to (body:mu/(s_r^2)) - ((w^2)*s_r).
			set s_pitch to A1 + (s_pitch / s_acc).
			set s_pitch1 to s_pitch.
			set s_pitch to max(-0.707, min(s_pitch, 0.707)). // limit the pitch change to between 1 and -1 with is -90 and 90 degress
			Set s_pitch to arcsin(s_pitch). //covert into degress check
			// If f_Tol(orbit:inclination, tgt_inc, 0.1){
			// 	LOCK STEERING TO heading(ship:heading, s_pitch).
			// }Else{
			// 	LOCK STEERING TO heading(f_FlightAzimuth(tgt_inc, tgt_vx), s_pitch).
			// }
            
        } 


        if converged = 1 {
			LOCK STEERING TO heading(f_FlightAzimuth(tgt_inc, tgt_vx), s_pitch).
			ClearScreen.
			Print "closed loop Steering".
			Print "Pitch: " + s_pitch.
			Print (A1 + B1*T1 + C1).
			Print "A: " + A1.
			Print "B: " + B1.
			Print "C1: " + C1.
			Print "T: " + T1.
			Print "tau: " + tau.
			Print "CT:" + peg[4].
			Print "dv:" + peg[5].
			Print "delta: " + delta.
			Print "missiontime: " + missiontime.
			Print abs(T1 - delta).
			Print "Tgt w: " + tgt_w.
			Print "r_w: " + r_w.
			Print "height: " + (r_w - body:radius).
			Print "vo: " + tgt_w * r_w.
			Print "rdot: " +rdot_w.
			Print "T2: " + T2.
			Print "A2: " +A2.
			Print "B2: " +B2.
			Print "C2: " +C2.
			Print arcsin(s_pitch1).
			Set s_pitch1 to (A1 + B1*delta + C1).
			Print s_pitch1.
			Print arcsin(s_pitch1).
			Print "Pitch at staging: " + arcsin(T2_new[1]+T2_new[2]).
			
            if(End-tc < 1) or (T1 <0.2){
                break. //break when the time left to burn minus the last step incriment is less than 0.2 seconds remaining so we do not enter that last few step(s) where decimal and estmation accuracy becomes vital.
            }
        }
        wait 0.01. 
		if(End-tc < 1){
			break. //break when the time left to burn minus the last step incriment is less than 0.2 seconds remaining so we do not enter that last few step(s) where decimal and estmation accuracy becomes vital.
		}
    }

} // end of function

///////////////////////////////////////////////////////////////////////////////////
function hf_PEG_ABT {
    parameter oldA.
    parameter oldB.
    parameter oldT.
    parameter delta.
	parameter tau.
	parameter tgt_vy.
	parameter tgt_vx.
	parameter tgt_r.
	parameter s_vy.
	parameter s_vx.
	parameter s_r.
	parameter s_acc.
    
	local A is 0.
    local B is 0.
    local C is 0.
    local T is 0.

	local s_ve is f_Vel_Exhaust().
    Local tgt_h is vcrs(v(tgt_r, 0, 0), v(tgt_vy, tgt_vx, 0)):mag.
	///if first time through get inital A and B values
    if oldA = 0 and oldB = 0 {
        local ab is hf_peg_solve(oldT, tau, tgt_vy, tgt_r, s_vy, s_r, s_ve).
        set oldA to ab[0].
        set oldB to ab[1].
    }
	local T_dash is oldT - delta.
    local h0 is vcrs(v(s_r, 0, 0), v(s_vy, s_vx, 0)):mag. //current angular momentum
    local dh is tgt_h - h0. //angular momentum to gain
	
    Local C is (body:mu/s_r^2 - (s_vx^2/s_r))/s_acc. //current portion of vehicle acceleration used to counteract gravity
	local CT is ((body:mu/tgt_r^2) - (tgt_vx^2/tgt_r)) / (s_acc / (1-(oldT/tau))). //Gravity and centrifugal force term at cutoff

    local fr is oldA + C. //sin pitch at current time	
	local frT is oldA + oldB*oldT + CT. //sin pitch at burnout.
	local frdot is (frT-fr)/oldT. //approximate rate of sin pitch

    local ft is 1 - (fr^2)/2. //cos pitch
    local ftdot is -fr*frdot. //cos pitch speed
    local ftdd is -(frdot^2)/2. //cos pitch acceleration

    local mean_r is (tgt_r + s_r)/2.
    local dv is (dh/mean_r) + ((s_ve*T_dash) * (ftdot+(ftdd*tau))) + ((ftdd*s_ve*(T_dash^2))/2). // ideal velocity to gain. note this is from nasa manual equation 36 and wiki
	//Print (ft + ftdot*tau + ftdd*(tau^2)).
    set dv to dv / (ft + (ftdot*tau) + (ftdd*(tau^2))). // big equation from wiki near end of estimated
	//Print "DV: " + dv.
    local T_plus is tau*(1 - constant:e ^ (-dv/s_ve)). // estimated updated burnout time
   //Print "T_plus" + T_plus.
	// if (t_plus-tau)/tau < 0.001{ //if the result is too close to a full burnout try again until the craft is in a better position to converge
	// 	//Print (t_plus-tau)/tau.
	// 	Set t_plus to T_dash*0.95. 
	// 	//Print "new T-Plus" + T_plus.
	// }
    if(T_plus >= 5) { // check above, below this the solution starts to become very sensitive and A and B should not longer be re-calculated
        local ab is hf_peg_solve(T_plus, tau, tgt_vy, tgt_r, s_vy, s_r, s_ve).
        set A to ab[0].
        set B to ab[1].
   } else {
        Print ("Terminal guidance enabled"). // keep A and B constants static
        set A to oldA.
        set B to oldB.
   }
	wait 0.01.
    return list(A, B, C, T_plus, CT, dv).
}
	


///////////////////////////////////////////////////////////////////////////////////

// Estimate, returns A and B coefficient for guidance
function hf_peg_solve {
    parameter T.//Estimated time until burnout
    parameter tau. // tau = ve/a which is the time to burn the vehicle completely if it were all propellant
	parameter tgt_vy.
	parameter tgt_r.
	parameter s_vy.
	parameter s_r.
	parameter s_ve.
	
    local b0 is -s_ve * LN(1 - (T/tau)). //Wiki eq 7a
    local b1 is (b0*tau) - (s_ve*T). //Wiki eq 7b
    local c0 is b0*T - b1. //Wiki eq 7c
    local c1 is (c0*tau) - (s_ve * T^2)/2. //Wiki eq 7d
    local mb0 is tgt_vy - s_vy.  //Wiki Major loop algortthm MB Matrix top
    local mb1 is (tgt_r - s_r) - s_vy*T. //Wiki Major loop algortthm MB Matrix bottom
    local d is (b0*c1 - b1*c0). // //Wiki Major loop algortthm intermediate stage to solve for Mx from Ma and Mb
    
    local B is (mb1/c0 - mb0/b0) / (c1/c0 - b1/b0). 
	local A is (mb0 - b1*B) / b0.

    return list(A, B).
}

///////////////////////////////////////////////////////////////////////////////////
//Estimate w

function hf_PEG_w {
    parameter A1.
    parameter B1.
    parameter T1.
	parameter s_acc_end.
	parameter tgt_w. /// this is what we are trying to determine
    
	local s_v is ship:verticalspeed.
	local s_r is ship:orbit:body:distance.
	local s_ve is f_Vel_Exhaust().
	local s_acc is ship:AVAILABLETHRUST/ship:mass. // current ship parameter
	local tau is s_ve/s_acc. //time to burn ship if all propellant

	local bb0 is -s_ve*(LN(1-(T1/tau))).
	local bb1 is (bb0 * tau) - (s_ve*T1).
	local bb2 is (bb1 * tau) - ((s_ve*(T1^2))/2).
	local cc0 is (bb0*T1)-bb1.
	local cc1 is (cc0*tau) - ((s_ve*(T1^2))/2).

	Local rdotT is s_v +bb0*A1 + bb1*B1. //vertical speed at staging
	Local rT is s_r + (s_v*T1) + (cc0 * A1) + (cc1*B1). // vertical distance at staging
    Local C is (body:mu/s_r^2 - (s_v^2/s_r))/s_acc. //portion of vehicle acceleration used to counteract gravity
	//Print "c:" + C.
	local fr is A1 + C. //sin pitch at current time
	Local hT is 0. //set up before loop to caputure value on exit of loop.
	local tgt_w_new is 0. //set up before loop to caputure value on exit of loop.
	until false {

		local CT is (body:mu/(rT^2)) - ((tgt_w^2)*rT). //Gravity and centrifugal force term at cutoff
		Set CT to CT /s_acc_end. 
		local frT is A1 + B1*T1 + CT. //sin pitch at burnout. Others code
		local frdot is (frT-fr)/T1. //approximate rate of sin pitch

		local ft is 1 - (fr^2)/2. //cos pitch
		local ftdot is -fr*frdot. //cos pitch speed
		local ftdd is -(frdot^2)/2. //cos pitch acceleration

		Set hT to ((s_r + rT)/2)*( (ft*bb0) + (ftdot*bb1) + (ftdd*bb2) ).
		Print hT.
		local v0 is hT/rT.
		Print v0.
		Set tgt_w_new to v0/rT.

		Print tgt_w_new.

		if f_Tol (tgt_w, tgt_w_new, abs(tgt_w*0.01)) {
			break.
		}else{
			set tgt_w to tgt_w_new.
		}
		wait 0.01.
	}
    return list (tgt_w_new, rT, rdotT, hT).
}

///////////////////////////////////////////////////////////////////////////////////
//Estimate dA and dB

function hf_PEG_dA_dB {
    parameter w1.
    parameter r1.
	parameter rdot1.
    parameter T1.
	parameter s_acc_end.
	parameter s2_acc_start.
	parameter s_ve2.
    
	local s_ve1 is f_Vel_Exhaust().

	Local dA is ( (body:mu/(r1^2)) - ((w1^2)*r1) ).
	Set dA to dA * ( (1/s_acc_end) - (1/s2_acc_start) ).
	Local dB is - ( (body:mu/(r1^2)) - ((w1^2)*r1) ) * ( (1/s_ve1) - (1/s_ve2)  ). 
	Set dB to dB + ( ( (3*(w1^2)) - ((2*body:mu)/(r1^3)) ) *rdot1* ( (1/s_acc_end) - (1/s2_acc_start) )  ).

	Print "dA: " + dA.
	Print "dB: " + dB.

    return list(dA, dB).
}

///////////////////////////////////////////////////////////////////////////////////
//Estimate T2

function hf_PEG_T2 {
    parameter dA.
    parameter dB.
	parameter hT1.
	parameter rT.
	parameter tgt_h.
    parameter T1.
	parameter s_acc_end.
	parameter s2_acc_start.
	parameter s_ve2.
	parameter tgt_w.
	parameter mass_flow.
	parameter start_mass.
	Parameter T2.
	Parameter A1.
	Parameter B1.
	Parameter tgt_r.
    
	local A2 is dA + A1 + (B1*T1).
	local B2 is dB + B1.

	local tau2 is s_ve2/s2_acc_start.
	local F2 is start_mass/s2_acc_start.

	local a2 is F2 / (start_mass - (mass_flow*T2)).

    local dh is tgt_h - hT1. //angular momentum to gain in next stage
    local mean_r is (tgt_r + rT)/2.
    Local C2 is (body:mu/(rT^2)) - ((tgt_w^2)*rT).
	Set C2 to C2/s2_acc_start. //current portion of vehicle acceleration used to counteract gravity
    local fr is A2 + C2. //sin pitch at start of next stage	
	local C2T is (body:mu/(rT^2)) - ( ((tgt_h/tgt_r)^2)*tgt_r ).
	Set C2T to C2T/a2.

	local frT is A2 + B2*T2 + C2T. //sin pitch at burnout.
	local frdot is (frT-fr)/T2. //approximate rate of sin pitch

    local ft is 1 - (fr^2)/2. //cos pitch
    local ftdot is -fr*frdot. //cos pitch speed
    local ftdd is -(frdot^2)/2. //cos pitch acceleration

    local dv is (dh/mean_r) + ((s_ve2*T2) * (ftdot+(ftdd*tau2))) + ((ftdd*s_ve2*(T2^2))/2). // ideal velocity to gain. note this is from nasa manual equation 36 and wiki
	Print (ft + ftdot*tau + ftdd*(tau^2)).
    set dv to dv / (ft + (ftdot*tau2) + (ftdd*(tau2^2))). // big equation from wiki near end of estimated
	Print "DV: " + dv.
    local T_plus is tau2*(1 - constant:e ^ (-dv/s_ve2)). // estimated updated burnout time
    Print "T_plus" + T_plus.
    return list(T_plus, A2, B2, C2).
}



function f_FlightAzimuth {
	parameter inc. // target inclination
    parameter V_orb. // target orbital speed
  
	// project desired orbit onto surface heading
	local az_orb is arcsin ( cos(inc) / cos(ship:latitude)).
	if (inc < 0) {
		set az_orb to 180 - az_orb.
	}
	
	// create desired orbit velocity vector
	local V_star is heading(az_orb, 0)*v(0, 0, V_orb).

	// find horizontal component of current orbital velocity vector
	local V_ship_h is ship:velocity:orbit - vdot(ship:velocity:orbit, up:vector)*up:vector.
	
	// calculate difference between desired orbital vector and current (this is the direction we go)
	local V_corr is V_star - V_ship_h.
	
	// project the velocity correction vector onto north and east directions
	local vel_n is vdot(V_corr, ship:north:vector).
	local vel_e is vdot(V_corr, heading(90,0):vector).
	
	// calculate compass heading
	local az_corr is arctan2(vel_e, vel_n).
	return az_corr.

}// End of Function

function f_Vel_Exhaust {
	local g is 9.80665.  // Gravitational acceleration constant used in game for Isp Calculation (m/s²)
	local engine_count is 0.
	local thrust is 0.
	local isp is 0. // Engine ISP (s)
	list engines in all_engines.
	for en in all_engines if en:ignition and not en:flameout {
	  set thrust to thrust + en:availablethrust.
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	set isp to isp / engine_count.
	return g *isp.///thrust). //
}/// End Function

function f_Tol {
//Calculates if within tolerance and returns true or false
	PARAMETER a. //current value
	PARAMETER b.  /// Setpoint
	PARAMETER tol.

	RETURN (a - tol < b) AND (a + tol > b).
}


function f_IGM_Steer_orbit{

/////////////////////////////////////////////////////////////////////////////////////
//Used on the final stage only

parameter End.
Parameter tgt_pe. //target periapsis
Parameter tgt_ap. //target apoapsis
Parameter tgt_inc. //target inclination
Parameter u is 0. // target true anomaly in degrees(0 = insertion at Pe)

//values setup
    set ra to body:radius + tgt_ap. //full Ap
    set rp to body:radius + tgt_pe. //full pe
    local sma is (ra+rp)/2. //sma
    local ecc is (ra-rp)/(ra+rp). //eccentricity
    local vp is sqrt((2*body:mu*ra)/(rp*2*sma)). // this is the target velocity at the periapsis
	Print "vp " +vp. 
    local rc is (sma*(1-ecc^2))/(1+ecc*cos(u)). // this is the target radius based on the desire true anomoly
    print "rc "+rc.
    local vc is sqrt((vp^2) + 2*body:mu*((1/rc)-(1/rp))). // this is the target velocity at the target radius (if u is zero this will equal vp)
    print "vc "+vc.
    local uc is 90 - arcsin((rp*vp)/(rc*vc)). // this is the direction vector of the target velocity
    // Define target position and velocities
    local tgt_r is rc.
    local tgt_vy is vc*sin(uc). // this is the split of the target velocity at the point in time
    local tgt_vx is vc*cos(uc). // this is the split of the target velocity at the point in time (should be zero for u = 0)
	Print "tgt_vx " + tgt_vx.
	Print "tgt_vy " + tgt_vy.
    Local tgt_h is vcrs(v(tgt_r, 0, 0), v(tgt_vy, tgt_vx, 0)):mag. // target angular momentum
	wait 1.0.
    Local tc to (time:seconds - liftoff). // current mission time progression since lift off
    local last is tc. //starting mission elased time at last point of calculation
    local A is 0. //starting peg variable
    local B is 0. //starting peg variable
    local C is 0. //starting peg variable
    local converged is -3.
    local delta is 0. //time between peg loops
	local T is 50. //intial guess on time to thrust cut off

	local peg_step is 1.0.//time between each calcuation check
    
    local s_r is ship:orbit:body:distance. // current ship parameter
    local s_acc is ship:AVAILABLETHRUST/ship:mass. // current ship parameter
	Print "s_acc " + s_acc.
    local s_vy is ship:verticalspeed. // current ship parameter
    local s_vx is sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2). // current ship parameter
	local s_ve is f_Vel_Exhaust().
	Print "Vel exhaust: " + s_ve.
	local w is s_vx / s_r.
	Set tau to s_ve/s_acc. //time to burn ship if all propellant
	Print "tau: " + tau.
    local peg is hf_PEG_ABT(A, B, T, peg_step, tau, tgt_vy, tgt_vx, tgt_r, s_vy, s_vx, s_r, s_acc).  // inital run through the cycle with first estimations
    wait 0.001.

    Print "Entering Convergence loop".
	//Loop through updating the parameters until the break condition is met
    until false {
		// collect current ship parameter
        Set tc to (time:seconds - liftoff).
        set s_r to ship:orbit:body:distance.
		set s_acc to ship:AVAILABLETHRUST/ship:mass.
		set s_vy to ship:verticalspeed.
		set s_vx to sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
		Set tau to s_ve/s_acc.
		Set w to s_vx / s_r.
        set delta to tc - last. // time since last calculation
		Print "Mission Time " + tc.
		Print "delta " + delta.
		Print "peg_step " + peg_step.
		
        If (delta >= peg_step) {  // this is used to ensure a minimum time step occurs before undertaking the next peg cycle
            //f_peg_vis().
			Print "Convergence Step#####################################################".
			Set peg to hf_PEG_ABT (A, B, T, peg_step, tau, tgt_vy, tgt_vx, tgt_r, s_vy, s_vx, s_r, s_acc).
			Set last to (time:seconds - liftoff).
			Print "Con Step percent: " + abs( (T-2*peg_step)/peg[3]-1 ).
			Print T.
			Print peg[3].
			Print delta.
			if abs( (T-2*peg_step)/peg[3]-1 ) < 0.02 {  //if the time returned is within 2% of the old T guess to burnout allow convergence to progress 
				//ClearScreen.
                if converged < 0 {
                    set converged to converged+1. //(this is done over 3 cycles to ensure the convergence solution selected is accurate enough over ten ship location updates rather than relying on only one convergence solution to enter a closed loop)
					Print "Convergence step +1".
                } else if converged = 0 {
                    set converged to 1.
                    Print("closed loop enabled").
                }
            } 

            set A to peg[0].
            set B to peg[1].
            set C to peg[2].
            set T to peg[3].
			Set delta to 0.
            
        } 
		Print "Method check".
		Print "s_pitch T " + (A + B*T + C). //wiki Estimation fr,T = A + B*T + C noting T in this instance is the T until the estimated next step so its really delta or just zero if you consider what it should be now.
		Print "s_pitch delta" + (A + B*delta + C). //wiki Estimation fr,T = A + B*T + C noting T in this instance is the T until the estimated next step so its really delta or just zero if you consider what it should be now. 
        //set s_pitch to (A + B*delta + C).
		set s_pitch to (body:mu/(s_r^2)) - ((w^2)*s_r).
		set s_pitch to A + (s_pitch / s_acc).
		set s_pitch to max(-0.1, min(s_pitch, 0.1)). // limit the pitch change to between 1 and -1 with is -90 and 90 degress
        Set s_pitch to arcsin(s_pitch). //covert into degress check
		Print "s_pitch final " + s_pitch.
		// If f_Tol(orbit:inclination, tgt_inc, 0.1){
		// 	LOCK STEERING TO heading(ship:heading, s_pitch).
		// }Else{
		// 	LOCK STEERING TO heading(f_FlightAzimuth(tgt_inc, tgt_vx), s_pitch).
		// }

        if converged = 1 {
			//LOCK STEERING TO heading(f_FlightAzimuth(tgt_inc, tgt_vx), s_pitch).
			LOCK STEERING TO heading(90, s_pitch).
			ClearScreen.
			Print "closed loop Steering".
			Print "Pitch: " + s_pitch.
			Print (A + B*T + C).
			Print "A: " + A.
			Print "B: " + B.
			Print "C: " + C.
			Print "T: " + T.
			Print "tau: " + tau.
			Print "CT:" + peg[4].
			Print "dv:" + peg[5].
			Print "delta: " + delta.
			Print "missiontime: " + missiontime.
			Print abs(T - delta).
			
            if(End-tc < 1) or (T <0.2){
                break. //break when the time left to burn minus the last step incriment is less than 0.2 seconds remaining so we do not enter that last few step(s) where decimal and estmation accuracy becomes vital.
            }
        }
        wait 0.1. 
		if(End-tc < 1){
			break. //break when the time left to burn minus the last step incriment is less than 0.2 seconds remaining so we do not enter that last few step(s) where decimal and estmation accuracy becomes vital.
		}
    }

} // end of function

