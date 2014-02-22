%PRT statistics Generator
%prt runs from 6:30 to 4:30 in normal mode = 43200s
%each hour is 3600s; halves = 1800s

clc
clear all
close all
warning('off', 'stats:random:ChangedParameters')

printz=0;
wbl_k=1.5;
wbl_lamb=20;
%wbl_peak=10;

hours=[-19800:3600:43200];
minute=60;

edale_peak_recieve=hours(8:16);
downtown_peak_recieve=hours(8:15)+30*minute;
wbl=zeros(5000,20);
for j=1:20
    rk=wbl_k+random('uniform',-0.5, 0.5);
    rl=wbl_lamb+random('uniform',-0.5, 0.5);
    rmode=rl*((rk-1)/rk)^(1/rk);
    for i=1:5000
        wbl(i,j)=-(random('weibull',rl, rk)-rmode);
    end
end

if printz==1
    for i=1:20
        hist(wbl(:,i),100)
        pause(1)
    end
end

edale_riders=[]; bh_riders=[];
for phour=edale_peak_recieve
    mask=mod(1:5000,randi(20)+40)==0;
    edale_riders=[edale_riders; phour+wbl(mask,randi(20))*minute];
end
for phour=downtown_peak_recieve
    mask=mod(1:5000,randi(20)+40)==0;
    bh_riders=[bh_riders; phour+wbl(mask,randi(20))*minute];
end

for i=1:50+randi(50)
    edale_riders=[edale_riders; random('uniform',hours(6)+30*minute, hours(16)+30*minute)];
end
for i=1:50+randi(50)
    bh_riders=[bh_riders; random('uniform',hours(6)+30*minute, hours(16)+30*minute)];
end

while edale_riders(1)<0
    edale_riders=edale_riders(2:end);
end
while bh_riders(1)<0
    bh_riders=bh_riders(2:end);
end

while edale_riders(end)>=hours(16)+30*minute
    edale_riders=edale_riders(1:end-1);
end
while bh_riders(end)>=hours(16)+30*minute
    bh_riders=bh_riders(1:end-1);
end



disp(length(edale_riders))
disp(length(bh_riders))

subplot(211)
hist(bh_riders/60,(hours(16)+30*minute))
title('ENGR to Beechurst')
xlim([0,(hours(16)+30*minute)/minute])
subplot(212)
hist(edale_riders/60,(hours(16)+30*minute))
title('Beechurst to ENGR')
xlim([0,(hours(16)+30*minute)/minute])

ne=hist(edale_riders,hours(16)+30*minute);
nb=hist(bh_riders,hours(16)+30*minute);

phil=fopen('out.q','w');
for i=1:length(ne)
    if ne(i)>0
        fprintf(phil,'(add-people beechurst engineering_goer %u)',ne(i));
    end
    if nb(i)>0
        fprintf(phil,'(add-people engineering beechurst_goer %u)',nb(i));
    end
    fprintf(phil,'\n');
end
fclose(phil)