libname factor 'C:\Lan\Teaching\DSO 599 Marketing Analytics\Course Material Lan Luo\Cases\Bookbinder Case\'; 
run;
proc contents data = factor.bookbinderh position;
run;
proc means data = factor.bookbinderh; 
run;
/* Sets choice for holdout sample to . so it won't be included in data analysis */
data factor.bookbinderh1; 
  set factor.bookbinderh; 
  all = 1; 
  if choice = 1 then responder = 1; else responder = 0; 
  if holdout = 1 then choice1 = . ; else choice1 = choice; 
  if holdout = 0 then train = 1; else train = 0;
  if holdout ne . ;
run;

/* Runs model on training sample then writes predicted values for both samples to a file */
proc logistic data = factor.bookbinderh1 descending;
  model Choice1 = Gender	Amount_purchased	FREQUENCy	
    LAST_purchase	FIRST_purchase	P_Child	P_Youth	P_Cook	P_DIY	P_Art/
    rsquare ;
  output out = logscore1 p = phat;
  options ps =32000;
run;
/*Routine to create gains charts for training sample and holdout sample */
proc print data = logscore1; 
 options ps = 32000;
run;
 
/*Ranks training sample on phat */
proc rank data = logscore1 out = rankings groups = 10 descending ; 
  var phat; 
  ranks decile;
  where train = 1;
  run; 
data rankings; 
  set rankings; 
  all = 1;
run;

proc sort data = rankings;
 by decile descending phat ;
run; 
/*Creates cutoffs for deciles */
data vigintile (keep = all decile1-decile10) ; 
  set rankings; 
  by decile descending phat; 
  retain decile1-decile10 ; 
  if first.decile and decile = 0 then decile1 = phat; 
     else if first.decile and decile = 1 then decile2 = phat; 
     else if first.decile and decile = 2 then decile3 = phat; 
     else if first.decile and decile = 3 then decile4 = phat; 
     else if first.decile and decile = 4 then decile5 = phat; 
     else if first.decile and decile = 5 then decile6 = phat; 
     else if first.decile and decile = 6 then decile7 = phat;
     else if first.decile and decile = 7 then decile8 = phat; 
     else if first.decile and decile = 8 then decile9 = phat; 
     else if first.decile and decile = 9 then decile10 = phat; 
     if last.decile and decile = 9 then output vigintile;
run;
/* Outputs average values of phat for each decile - check that program is ok */ 
proc print data = vigintile; 
  var decile1-decile10; 
run;
/*Assigns training sample to deciles */
data log_train log_test; 
   merge logscore1 vigintile; 
   by all; 
   if phat gt decile2 then decile = 1; 
   if phat gt decile3 and phat le decile2 then decile = 2; 
   if phat gt decile4 and phat le decile3 then decile = 3; 
   if phat gt decile5 and phat le decile4 then decile = 4;   
   if phat gt decile6 and phat le decile5 then decile = 5; 
   if phat gt decile7 and phat le decile6 then decile = 6; 
   if phat gt decile8 and phat le decile7 then decile = 7; 
   if phat gt decile9 and phat le decile8 then decile = 8;   
   if phat gt decile10 and phat le decile9 then decile = 9; 
   if phat le decile10 then decile = 10;
   if train = 0 then output  log_test; 
       else output  log_train; 
run; 
proc summary data = log_train; 
  var responder all; 
 output out = avtrain mean (responder all) = avg_rate all; 
  run; 
run;
/*Summary stats by decile */
proc summary data = log_train nway; 
 class decile; 
 var responder all; 
 output out = gains1 (keep = decile mail response resp_rate all)
   n = mail sum = response mean = resp_rate all;
  run; 
data gains1; 
  merge gains1 avtrain; 
  by all; 
run;

proc sort data = gains1;
  by decile; 
run; 
/* Creates the gains chart */
data gains1; 
 set gains1; 
 by decile; 
 retain cummail cumresp;
 cummail + mail; 
 cumresp + response; 
 cum_resp_rate = cumresp/cummail; 
 index = 100*(resp_rate / avg_rate);
 cum_index = 100*(cum_resp_rate / avg_rate);
 run; 
proc print data = gains1 noobs split = "*" double ; 
 var decile mail response resp_rate cummail cumresp index cum_index ; 
 title "Gains chart for logistic layer"; 
 title "Analysis Sample"; 
 Label decile = "response*decile"
        mail = "mail*quantity*in decile"
        response = "responses*in decile"
        resp_rate = "response*rate*for*decile"
        cummail = "cumulative*mail*quantity"
        cumresp = "cumulative*responses" 
		index = "index*response*in decile"
        cum_index = "cumulative*index response*through*decile"; 
run; 
proc rank data = logscore1 out = rankings groups = 10 descending ; 
  var phat; 
  ranks decile;
  where train = 0;
  run; 
data rankings; 
  set rankings; 
  all = 1;
run;

proc sort data = rankings;
 by decile descending phat ;
run; 
/*Creates cutoffs for deciles */
data vigintile (keep = all decile1-decile10) ; 
  set rankings; 
  by decile descending phat; 
  retain decile1-decile10 ; 
  if first.decile and decile = 0 then decile1 = phat; 
     else if first.decile and decile = 1 then decile2 = phat; 
     else if first.decile and decile = 2 then decile3 = phat; 
     else if first.decile and decile = 3 then decile4 = phat; 
     else if first.decile and decile = 4 then decile5 = phat; 
     else if first.decile and decile = 5 then decile6 = phat; 
     else if first.decile and decile = 6 then decile7 = phat;
     else if first.decile and decile = 7 then decile8 = phat; 
     else if first.decile and decile = 8 then decile9 = phat; 
     else if first.decile and decile = 9 then decile10 = phat; 
     if last.decile and decile = 9 then output vigintile;
run;
/* Outputs average values of phat for each decile - check that program is ok */ 
proc print data = vigintile; 
  var decile1-decile10; 
run;
/*Assigns training sample to deciles */
data log_train log_test; 
   merge logscore1 vigintile; 
   by all; 
   if phat gt decile2 then decile = 1; 
   if phat gt decile3 and phat le decile2 then decile = 2; 
   if phat gt decile4 and phat le decile3 then decile = 3; 
   if phat gt decile5 and phat le decile4 then decile = 4;   
   if phat gt decile6 and phat le decile5 then decile = 5; 
   if phat gt decile7 and phat le decile6 then decile = 6; 
   if phat gt decile8 and phat le decile7 then decile = 7; 
   if phat gt decile9 and phat le decile8 then decile = 8;   
   if phat gt decile10 and phat le decile9 then decile = 9; 
   if phat le decile10 then decile = 10;
   if train = 0 then output  log_test; 
       else output  log_train; 
run; 


proc summary data = log_test; 
  var responder all; 
 output out = avtest mean (responder all) = avg_rate all; 
  run; 
run;
/* Summary stats for holdout sample */ 
proc summary data = log_test nway; 
 class decile; 
 var responder all; 
 output out = gains2 (keep = decile mail response resp_rate all)
   n = mail sum = response mean = resp_rate all;
  run; 

/* Gains chart for holdout sample */
data gains2; 
  merge gains2 avtest; 
  by all; 
run;
proc sort data = gains2;
  by decile; 
  run; 
data gains2; 
 set gains2; 
 by decile; 
 retain cummail cumresp;
 cummail + mail; 
 cumresp + response;
 cum_resp_rate = cumresp/cummail; 
 index = 100*(resp_rate / avg_rate);
 cum_index = 100*(cum_resp_rate / avg_rate);
 resp_rate1 = response/204;
 run; 
proc print data = gains2 noobs split = "*" double ; 
 var decile mail response resp_rate1 cummail cumresp index cum_index ; 
 title "Gains chart for logistic layer"; 
 title "Validation Sample"; 
 Label decile = "response*decile"
        mail = "mail*quantity*in decile"
        response = "responses*in decile"
        resp_rate1 = "response*rate"
        cummail = "cumulative*mail*quantity"
        cumresp = "cumulative*responses" 
		index = "index*response*in decile"
        cum_index = "cumulative*index response*through*decile"; 
run;  


