;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;     MORSE TUTOR V1.2
;
;     Modified:   November 23rd, 2014
;     		a) Changed the delay range to be upto 5 seconds.
;
;     Modified:   November 26th, 2014
;     		a) If the user has added a delay between characters then we
;     		force the word gap to be 5 seconds.
;
;     Modified:   June 28th, 2016
;               a) Bug Fix. Many thanks to Jan Langevad, OZ8MS, who found a
;               bug with the skipping over digits when sending just alphabetic
;               charaters and punctuation.
;
;     Copyright: M.D.Waller - G0PJO (c) 2014
;
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
; Switch related symbols. With the three switches to select the characters
; to be generated we have seven possible combinations made up from Letters,
; Numbers, and Punctuation. The following represent all possible
; combinations and will be used to keet track of the switch positions.
;
symbol	L = 1 					;Letters only	
symbol	N = 2 					;Numbers only
symbol	P = 4 					;Punctuation only
symbol	LN = 3					;Letters and Numbers
symbol	LP = 5					;Letters and Punctuation
symbol  NP = 6					;Numbers and Punctuation
symbol	LNP = 7     				;Letters, Numbers and Punctuation
;
; The EEPROM is used to hold a number of lookup tables and data that we need
; to preserve across the tutor use. These symbols are used to give base addresses
; within the EEPROM where data can be found.
;
; The First group are used to index the Morse code characters.
;
symbol	EEPROMLETTERSTART = 0  		 	;Base address for Letters
symbol  EEPROMLETTERLENGTH = 26 		;Number of Letters
symbol  EEPROMDIGITSTART = 26        	 	;Base address for Digits
symbol  EEPROMDIGITLENGTH = 10  		;Number of Digits
symbol  EEPROMPUNCTUATIONSTART = 36         	;Base address for punctuation
symbol  EEPROMPUNCTUATIONBASICLENGTH = 8    	;Number of Basic Punctuation characters
symbol	EEPROMPUNCTUATIONEXTENDLENGTH = 18  	;Number of Basic + Extended Punctuation characters
;
; To translate the Speed potentiometer position into a speed position we
; make use of a lookup table. The following symbols gives the base address
; of this table.
;
symbol	EEPROMSPEEDLOOKUP = 108
;
; To translate the Delay potentiometer position into a delay position we
; make use of a lookup table. The following symbols gives the base address
; of this table.
;
symbol	EEPROMDELAYLOOKUP = 134
;
; To make the characters random we need to preserve the seed across power ups.
; If we don't we'll always end up starting with the same character sequence!
;
symbol  EEPROMRANDOMSEED = 200
;
; The following symbols define the hardware pins used by the tutor +++
;
symbol	LETTERS_PIN = pinb.0
symbol	DIGITS_PIN = pinb.1
symbol	PUNCTUATION_PIN = pinb.2

symbol	OSCILLATOR_PIN = c.0
symbol	LED_PIN = b.4
symbol      SPEED_PIN = b.7
symbol	DELAY_PIN = B.5
;
; The following symbols allocate names variables to the PIC registers
;
symbol	mc = b0     				;Morse code character
symbol	mcl = b1    				;Morse code character length
symbol  i = b2    				;Looping variable
symbol  rr = b3     				;Character index
symbol	switchValue = b4  			;Switch value
symbol	offset = b5 				;Offset to the characters to send
symbol  length = b6 				;Length of the characters to send
symbol  groupIndex = b7   			;5 figure group index
symbol  RepeatCount = b8			;Repeat count
symbol  mcc = b9
symbol mclc = b10

symbol  FiveSMS = w7    			;5 Second milliseconds
symbol  DelayMS = w8				;Delay milliseconds
symbol  DitLength = w9    			;Dit length milliseconds
symbol  DahLength = w10   			;Dah length milliseconds
symbol  LetterSpace = w11 			;Letter spacing milliseconds
symbol  WordSpace = w12   			;Word spacing milliseconds

symbol  randomNumber = w13
;
; This is the main line code! Start by making sure we know what state the
; hardware is in.
;
    FiveSMS = 5 * 1000  			    ;Load the 5 Second count
    LOW   OSCILLATOR_PIN    			;Turn the oscillator off
    LOW   LED_PIN     				;Turn the LED off
    PULLUP %00000111  				;Turn on the pullup resistors for the switches
;
MAIN_LOOP:
;
; We are going to send out characters in groups of 5. Between each group of
; 5 we will send out a word spacing. Basiclaly we send out 5 letter random
; words (very random!).
;
    FOR groupIndex = 1 TO 5
;
; We need to know how many times we are going to repeat the character. This
; is determined by the delay potentiometer.
;
	READADC DELAY_PIN,b1    			;Read the potentiometer
	LET b1 = b1 / 10      		  		;Divide it by 10 - 0,255 / 10 gives a number in our range
	LET b1 = b1 * 2   				;Two bytes per delay
	LET RepeatCount = 1				;Assume a repeat count of 1
	IF b1 >= 30 THEN 					;If > 3 seconds... 		    	
	    LET RepeatCount = 3				;...repeat 3 times
	ELSE  					
	    IF b1 >= 10 THEN 				;If > 1 second...	
	        LET RepeatCount = 2			;...repeat twice
	    ENDIF
	ENDIF	
	LET b1 = b1 + EEPROMDELAYLOOKUP     	;Add the base address of the lookup table
	READ b1,WORD DelayMS 	 			;Read the Delay in ms
;
; Now we can read the speed potentiometer to determine what speed we
; need to send the Morse at. We'll do this before each and every character
; to mnake it more responsive.
;
	READADC SPEED_PIN,b1    		;Read the potentiometer
	LET b1 = b1 / 10  			;Divide it by 10 - 0,255 / 10 gives a number in our range
	LET b1 = b1 + EEPROMSPEEDLOOKUP 	;Add the base address of the lookup table
	READ b1,b1  				;Pull out the DitLength
;
; Now we have the DitLength we can work out the DahLength, the letter spacing
; and the word spacing.
;
	LET DitLength = b1			;Save the Dit length
	LET DahLength = 3 * DitLength 		;Calculate the Dah length
	LET LetterSpace = 3 * DitLength     	;Calulate the Letter spacing
	LET WordSpace = 7 * DitLength	 	;Calculate the Word spacing
;
; Now we need a random number. We load the seed from the EEPROM, generate a new
; random number and then write that back to be used as the seed next time round.
;		
	READ EEPROMRANDOMSEED, WORD randomNumber	;Read the last random number
	RANDOM randomNumber    				;Generate a random number
	WRITE EEPROMRANDOMSEED, WORD randomNumber	;Save the new random number
;
; Now we need to determine the state of the character switches. We'll check
; each in turn and add in the associated value which will result in one value
; providing the combination selected.
;		
	LET switchValue = 0     		;Clear the switch value
	IF LETTERS_PIN = 0 THEN 		;Letters selected
	    LET switchValue = switchValue + L   ;Add in the Letter constant
	ENDIF
	IF DIGITS_PIN = 0 THEN  		;Digits selected
	    LET switchValue = switchValue + N   ;Add in the Digit constant
	ENDIF
	IF PUNCTUATION_PIN = 0 THEN   		;Punctuation selected
	    LET switchValue = switchValue + P   ;Add in the Punctuation constant
	ENDIF	
;
; This is where the fun starts. We need to work out where in the EEPROM we'll be
; reading our random character from. This may come from a contiguous group of
; characters but in the case of Letters and Punctuation from a non contiguous
; group of characters.	
;		
	LET offset = EEPROMLETTERSTART			;Assume the start of the Letters
	SELECT CASE switchValue 			;Decode the switch value
	    CASE L  					;Just Letters
	        LET length = EEPROMLETTERLENGTH   	;Set the length to the Letter count
				  
	    CASE N  					;Just Numbers
		LET offset = EEPROMDIGITSTART     	;Set the offset to the start of the Digits
		LET length = EEPROMDIGITLENGTH    	;Set the length to the Digit count
				  
	    CASE LN
		LET length = EEPROMLETTERLENGTH + EEPROMDIGITLENGTH
			  
	    CASE P  					;Just Punctuation
		LET offset = EEPROMPUNCTUATIONSTART     ;Set the offset to the start of the Punctuation
		LET length = EEPROMPUNCTUATIONBASICLENGTH
				      			;Set the length to the Punctuation count
	    CASE LP
		LET length = EEPROMLETTERLENGTH + EEPROMPUNCTUATIONBASICLENGTH
				  
	    CASE NP 					;Number and Punctuation
		LET offset = EEPROMDIGITSTART     	;Set the offset to the start of the Digits
		LET length = EEPROMDIGITLENGTH + EEPROMPUNCTUATIONBASICLENGTH
			      				;Set the length to the Digit count plus Punctuation count
	    CASE LNP					;Letters, Digits and Punctuation
		LET length = EEPROMLETTERLENGTH + EEPROMDIGITLENGTH + EEPROMPUNCTUATIONBASICLENGTH
			      				;Set the length to cover all
	    ELSE    					;Otherwise, all switches off assume all
		LET length = EEPROMLETTERLENGTH + EEPROMDIGITLENGTH + EEPROMPUNCTUATIONEXTENDLENGTH 
			      				;Set the length to cover all
	ENDSELECT
;
; We now have an idea of where we need to choose our character from.
;
	LET rr = randomNumber % length     		;Take the remainder after dividing by the length
	IF switchValue <> LP THEN 		    	;We have a contiguous group
	    LET rr = offset + rr			;Add the offset
	ELSE  						;We have a split group
	    IF rr > EEPROMLETTERLENGTH THEN 		;If we need to move to the Punctuation
	        LET rr = rr + EEPROMDIGITLENGTH         ;Add of the offset - Bug fix June 28th, 2016
	    ENDIF
	ENDIF
;
; We now have the index into the EEPROM from which we can pull out Morse character.
; We need to pull out two values, the encoded character and the length of the encoded
; character.
;
	LET rr = rr * 2   				;Multipy by 2 - 2 bytes per character
	READ rr,mc  					;Read the Morse character
	LET rr = rr + 1   				;Move up one
	READ rr,mcl 					;Read the Morse character length
;
; Copy the Morse character and the length so we can re-use them.
;
	let mcc = mc
	let mclc = mcl
;
; We can now repeat as required.
;
	DO 
	    LET mc = mcc					;Copy in the character
	    LET mcl = mclc				;Copy in the character length
	    GOSUB SendMorse   				;Send it
	    PAUSE DelayMS    				;And delay
	    IF groupIndex < 5 THEN  			;If not at the end of the word
	        GOSUB LetterGap     			;Send the Letter spacing
	    ENDIF
	    LET repeatcount = repeatcount - 1	;Reduce the repeat count
	LOOP WHILE repeatCount > 0			;And loop if required
;
    NEXT groupIndex
;
; Send out the word spacing
; 
;     If we have a value in DelayMS then the user is injecting a space between
;     characters. If that's the case then the normal WordGap can make no sence
;     as it's probably short compared to the gap as is likely to get lost. What
;     to do?
;
;     For the moment, if a extra spacing is being injected then we will delay
;     for 5 seconds!
;
    IF DelayMS = 0 THEN
        GOSUB WordGap     				;Send the Word spacing
    ELSE
	  PAUSE FiveSMS   				;Send the 5 second delay
    ENDIF
    
;
; And repeat!
;
    GOTO MAIN_LOOP
		  
    EEPROM 000,(064,002)    ;'A' -> .-
    EEPROM 002,(128,004)    ;'B' -> -...
    EEPROM 004,(160,004)    ;'C' -> -.-.
    EEPROM 006,(128,003)    ;'D' -> -..
    EEPROM 008,(000,001)    ;'E' -> .
    EEPROM 010,(032,004)    ;'F' -> ..-.
    EEPROM 012,(192,003)    ;'G' -> --.
    EEPROM 014,(000,004)    ;'H' -> ....
    EEPROM 016,(000,002)    ;'I' -> ..
    EEPROM 018,(112,004)    ;'J' -> .---
    EEPROM 020,(160,003)    ;'K' -> -.-
    EEPROM 022,(064,004)    ;'L' -> .-..
    EEPROM 024,(192,002)    ;'M' -> --
    EEPROM 026,(128,002)    ;'N' -> -.
    EEPROM 028,(224,003)    ;'O' -> ---
    EEPROM 030,(096,004)    ;'P' -> .--.
    EEPROM 032,(208,004)    ;'Q' -> --.-
    EEPROM 034,(064,003)    ;'R' -> .-.
    EEPROM 036,(000,003)    ;'S' -> ...
    EEPROM 038,(128,001)    ;'T' -> -
    EEPROM 040,(032,003)    ;'U' -> ..-
    EEPROM 042,(016,004)    ;'V' -> ...-
    EEPROM 044,(096,003)    ;'W' -> .--
    EEPROM 046,(144,004)    ;'X' -> -..-
    EEPROM 048,(176,004)    ;'Y' -> -.--
    EEPROM 050,(192,004)    ;'Z' -> --..
	
    EEPROM 052,(120,005)    ;'1' -> .----
    EEPROM 054,(056,005)    ;'2' -> ..---
    EEPROM 056,(024,005)    ;'3' -> ...--
    EEPROM 058,(008,005)    ;'4' -> ....-
    EEPROM 060,(000,005)    ;'5' -> .....
    EEPROM 062,(128,005)    ;'6' -> -....
    EEPROM 064,(192,005)    ;'7' -> --...
    EEPROM 066,(224,005)    ;'8' -> ---..
    EEPROM 068,(240,005)    ;'9' -> ----.
    EEPROM 070,(248,005)    ;'0' -> -----
	
    EEPROM 072,(080,005)    ;'+' -> .-.-.
    EEPROM 074,(132,006)    ;'-' -> -....-
    EEPROM 076,(204,006)    ;',' -> --..--
    EEPROM 078,(084,006)    ;'.' -> .-.-.-
    EEPROM 080,(144,005)    ;'/' -> -..-.
    EEPROM 082,(136,005)    ;'=' -> -...-
    EEPROM 084,(048,006)    ;'?' -> ..--..
    EEPROM 086,(224,006)    ;':' -> ---...
	
    EEPROM 088,(072,006)    ;'"' -> .-..-.
    EEPROM 090,(018,007)    ;'$' -> ...-..-
    EEPROM 092,(120,006)    ;''' -> .----.
    EEPROM 094,(176,005)    ;'(' -> -.--.
    EEPROM 096,(180,006)    ;')' -> -.--.-
    EEPROM 098,(168,006)    ;';' -> -.-.-.
    EEPROM 100,(104,006)    ;'@' -> .--.-.
    EEPROM 102,(052,006)    ;'_' -> ..--.-
    EEPROM 104,(080,006)    ;'<Para>' -> .-.-..
    EEPROM 106,(224,004)    ;'!' -> ---.
	
    EEPROM 108,(126)  ;Speed = 8 WPM, Dit Length = 126.85ms
    EEPROM 109,(113)  ;Speed = 8 WPM, Dit Length = 113.08ms
    EEPROM 110,(102)  ;Speed = 9 WPM, Dit Length = 102.01ms
    EEPROM 111,(092)  ;Speed = 10 WPM, Dit Length = 92.91ms
    EEPROM 112,(085)  ;Speed = 11 WPM, Dit Length = 85.30ms
    EEPROM 113,(078)  ;Speed = 12 WPM, Dit Length = 78.84ms
    EEPROM 114,(073)  ;Speed = 13 WPM, Dit Length = 73.30ms
    EEPROM 115,(068)  ;Speed = 14 WPM, Dit Length = 68.48ms
    EEPROM 116,(064)  ;Speed = 15 WPM, Dit Length = 64.25ms
    EEPROM 117,(060)  ;Speed = 16 WPM, Dit Length = 60.52ms
    EEPROM 118,(057)  ;Speed = 17 WPM, Dit Length = 57.20ms
    EEPROM 119,(054)  ;Speed = 18 WPM, Dit Length = 54.22ms
    EEPROM 120,(051)  ;Speed = 19 WPM, Dit Length = 51.54ms
    EEPROM 121,(049)  ;Speed = 20 WPM, Dit Length = 49.11ms
    EEPROM 122,(046)  ;Speed = 21 WPM, Dit Length = 46.90ms
    EEPROM 123,(044)  ;Speed = 22 WPM, Dit Length = 44.88ms
    EEPROM 124,(043)  ;Speed = 23 WPM, Dit Length = 43.02ms
    EEPROM 125,(041)  ;Speed = 24 WPM, Dit Length = 41.32ms
    EEPROM 126,(039)  ;Speed = 25 WPM, Dit Length = 39.74ms
    EEPROM 127,(038)  ;Speed = 26 WPM, Dit Length = 38.28ms
    EEPROM 128,(036)  ;Speed = 27 WPM, Dit Length = 36.92ms
    EEPROM 129,(035)  ;Speed = 28 WPM, Dit Length = 35.66ms
    EEPROM 130,(034)  ;Speed = 29 WPM, Dit Length = 34.48ms
    EEPROM 131,(033)  ;Speed = 30 WPM, Dit Length = 33.37ms
    EEPROM 132,(032)  ;Speed = 31 WPM, Dit Length = 32.34ms
    EEPROM 133,(031)  ;Speed = 32 WPM, Dit Length = 31.36ms
	
    EEPROM 134,(000,000)    ;Delay = 0.000 Seconds, 0ms
    EEPROM 136,(200,000)    ;Delay = 0.200 Seconds, 200ms
    EEPROM 138,(144,001)    ;Delay = 0.400 Seconds, 400ms
    EEPROM 140,(088,002)    ;Delay = 0.600 Seconds, 600ms
    EEPROM 142,(032,003)    ;Delay = 0.800 Seconds, 800ms
    EEPROM 144,(232,003)    ;Delay = 1.000 Seconds, 1000ms
    EEPROM 146,(176,004)    ;Delay = 1.200 Seconds, 1200ms
    EEPROM 148,(120,005)    ;Delay = 1.400 Seconds, 1400ms
    EEPROM 150,(064,006)    ;Delay = 1.600 Seconds, 1600ms
    EEPROM 152,(008,007)    ;Delay = 1.800 Seconds, 1800ms
    EEPROM 154,(208,007)    ;Delay = 2.000 Seconds, 2000ms
    EEPROM 156,(152,008)    ;Delay = 2.200 Seconds, 2200ms
    EEPROM 158,(096,009)    ;Delay = 2.400 Seconds, 2400ms
    EEPROM 160,(040,010)    ;Delay = 2.600 Seconds, 2600ms
    EEPROM 162,(240,010)    ;Delay = 2.800 Seconds, 2800ms
    EEPROM 164,(184,011)    ;Delay = 3.000 Seconds, 3000ms
    EEPROM 166,(128,012)    ;Delay = 3.200 Seconds, 3200ms
    EEPROM 168,(072,013)    ;Delay = 3.400 Seconds, 3400ms
    EEPROM 170,(016,014)    ;Delay = 3.600 Seconds, 3600ms
    EEPROM 172,(216,014)    ;Delay = 3.800 Seconds, 3800ms
    EEPROM 174,(160,015)    ;Delay = 4.000 Seconds, 4000ms
    EEPROM 176,(104,016)    ;Delay = 4.200 Seconds, 4200ms
    EEPROM 178,(048,017)    ;Delay = 4.400 Seconds, 4400ms
    EEPROM 180,(248,017)    ;Delay = 4.600 Seconds, 4600ms
    EEPROM 182,(192,018)    ;Delay = 4.800 Seconds, 4800ms
    EEPROM 184,(136,019)    ;Delay = 5.000 Seconds, 5000ms

    END
;
;+-----------------------------------------------------------------
;
;     ROUTINE:    SendMorse(mc)
;
;     This routine is called to send the given letter in Morse
;     code. The letter is stored in mc and mc will be destroyed
;     on exit from the routine. The number of parts making up
;     the character is stored in mcl.
;
SendMorse: FOR i = 1 TO mcl   			;For all the signals
		    IF mc >= 128 THEN DoDah   	;If we need a dah do it
		    GOSUB SendDit 		;Send a dit
		    GOTO DoSignalGap    	;Move on
DoDah:	    GOSUB SendDah 			;Send a dah
DoSignalGap:    IF i = mcl THEN DoNext    	;If last signal then skip the gap
		    GOSUB SignalGap     	;Space it out
		    LET mc = mc * 2     	;Update the letter
DoNext:     NEXT
		RETURN				;Return
;
;+-----------------------------------------------------------------
;
;     ROUTINE:    SendDit()
;
;     This routine is called to send a Morse code Dit.
;
SendDit:    HIGH  OSCILLATOR_PIN    		;Turn the oscillator on
	    HIGH  LED_PIN     			;Turn the LED on
	    PAUSE DitLength        		;Delay for a dah
	    LOW   OSCILLATOR_PIN    		;Turn the oscillator off
	    LOW   LED_PIN     			;Turn the LED off
	    RETURN				;Return
;
;+-----------------------------------------------------------------
;
;     ROUTINE:    SendDah()
;
;     This routine is called to send a Morse code Dah.
;
SendDah:    HIGH  OSCILLATOR_PIN     	     	;Turn the oscillator on
	    HIGH  LED_PIN			;Turn the LED on
	    PAUSE DahLength  			;Delay for a dah
	    LOW   OSCILLATOR_PIN		;Turn the oscillator off
	    LOW   LED_PIN 			;Turn the LED off
	    RETURN				;Return
;
;+-----------------------------------------------------------------
;
;     ROUTINE:    SignalSpace()
;
;     This routine is called to delay the processing by the correct
;     amount of time for the inter-signal gap.
;
SignalGap:  PAUSE DitLength   			;Pause as necessary
	    RETURN				;Return
;
;+-----------------------------------------------------------------
;
;     ROUTINE:    LetterSpace()
;
;     This routine is called to delay the processing by the correct
;     amount of time for the inter-letter gap.
;
LetterGap:  PAUSE LetterSpace 			;Pause as necessary
	    RETURN				;Return
;
;+-----------------------------------------------------------------
;
;     ROUTINE:    WordSpace()
;
;     This routine is called to delay the processing by the correct
;     amount of time for the inter-word gap.
;
WordGap:    PAUSE WordSpace   			;Pause as necessary
	    RETURN				;Return



