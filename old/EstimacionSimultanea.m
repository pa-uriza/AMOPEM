% Load electricity prices
load('Colombia_electricity_prices.mat');
X_M5=[X_Total(:,1:16),X_Total(:,27:end-1)];              %Parte determinística mes con dummmies
X_M6=[X_Total(:,1:26),X_Total(:,38)];                    %Parte determinística con fourier
X_M7=[X_Total(:,1:16),X_Total(:,27:end)];                %Parte determinística mes con dummmies y niño
X_M8=[X_Total(:,1:26),X_Total(:,38:39)];                 %Parte determinística mes con fourier y niño
%X_M8=[X_Total(:,1:26),X_Total(:,38),exp(X_Total(:,39))]; %%Parte determinística mes con fourier y exp(niño)
fromTrain=datenum('01-Jan-1997 00:00:00');
toTrain=datenum('31-Dec-2013 23:00:00');
fromTest=datenum('01-Jan-2014 00:00:00');
toTest=datenum('31-Dic-2014 23:00:00');
% Create Train set
% logPrices_Train=log(PricesC((PriceDates>=fromTrain)&(PriceDates<=toTrain),:));
logPrices_Train=log(PricesC((PriceDates>=fromTrain)&(PriceDates<=toTrain),:));
X_TrainM5=X_M5((PriceDates>=fromTrain)&(PriceDates<=toTrain),:);
X_TrainM6=X_M6((PriceDates>=fromTrain)&(PriceDates<=toTrain),:);
X_TrainM7=X_M7((PriceDates>=fromTrain)&(PriceDates<=toTrain),:);
X_TrainM8=X_M8((PriceDates>=fromTrain)&(PriceDates<=toTrain),:);

% Create Test set
% logPrices_Test=log(PricesC((PriceDates>=fromTest)&(PriceDates<=toTest),:));
logPrices_Test=log(PricesC((PriceDates>=fromTest)&(PriceDates<=toTest),:));
X_TestM5=X_M5((PriceDates>=fromTest)&(PriceDates<=toTest),:);
X_TestM6=X_M6((PriceDates>=fromTest)&(PriceDates<=toTest),:);
X_TestM7=X_M7((PriceDates>=fromTest)&(PriceDates<=toTest),:);
X_TestM8=X_M8((PriceDates>=fromTest)&(PriceDates<=toTest),:);

% Plot electricity prices
figure;
plot(PriceDates, PricesC);
datetick();
title('Hourly Electricity Prices');
xlabel('Date');
ylabel('Constant Prices Aug 2016(COP/kWh)');

%% Obtain log of prices
logPrices = log(PricesC);

% Obtain annual time factors from dates
PriceDates_Train=PriceDates((PriceDates>=fromTrain)&(PriceDates<=toTrain));
PriceDates_Test=PriceDates((PriceDates>=fromTest)&(PriceDates<=toTest));
PriceTimes = yearfrac(PriceDates_Train(1), PriceDates_Train);

% Calibrate parameters for the seasonality model
global C
global Pt
global Pt_1

C = X_TrainM5;
seasonParam = C\logPrices_Train;

% Plot log price and seasonality line
figure;
subplot(2, 1, 1);
plot(PriceDates_Train, logPrices_Train);
datetick();
title('log(price) and Seasonality');
xlabel('Date');
ylabel('log(Prices)');
hold on;
plot(PriceDates_Train, C*seasonParam, 'r');
hold off;
legend('log(Price)', 'seasonality');

% Plot de-seasonalized log price
X = logPrices_Train;%-C*seasonParam;
subplot(2, 1, 2);
plot(PriceDates_Train, X);
datetick();
title('log(price) with Seasonality Removed');
xlabel('Date');
ylabel('log(Prices)');

% Prices at t, X(t)
Pt = X(2:end);

% Prices at t-1, X(t-1)
Pt_1 = X(1:end-1);

% Discretization for daily prices
dt = 1/8766;

%%
% PDF for discretized model
% mrjpdf = @(Pt, phi, mu_J, sigmaSq, sigmaSq_J, lambda,b0,bt,gs1,gc1,gs2,gc2,gs3,gc3,gs4,gc4,gs5,gc5,gs6,gc6,gs7,gc7,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,bd) ...
%     lambda.*exp((-(Pt-(b0*C(:,1)+bt*C(:,2)+gs1*C(:,3)+gc1*C(:,4)+gs2*C(:,5)+gc2*C(:,6)+gs3*C(:,7)+gc3*C(:,8)+...
%     gs4*C(:,9)+gc4*C(:,10)+gs5*C(:,11)+gc5*C(:,12)+gs6*C(:,13)+gc6*C(:,14)+gs7*C(:,15)+gc7*C(:,16)+...
%     m2*C(:,17)+m3*C(:,18)+m4*C(:,19)+m5*C(:,20)+m6*C(:,21)+m7*C(:,22)+m8*C(:,23)+m9*C(:,24)+m10*C(:,25)+...
%     m11*C(:,26)+m12*C(:,27)+bd*C(:,28))-phi.*Pt_1-mu_J).^2)./ ...
%     (2.*(sigmaSq+sigmaSq_J))).* (1/sqrt(2.*pi.*(sigmaSq+sigmaSq_J))) + ...
%     (1-lambda).*exp((-(Pt-a-phi.*Pt_1).^2)/(2.*sigmaSq)).* ...
%     (1/sqrt(2.*pi.*sigmaSq));
unos=C(2:end,1);
% mrjpdf = @(Pt, phi, mu_J, sigmaSq, sigmaSq_J, lambda, b0) ...
%     lambda.*exp((-(Pt-(b0*unos)-phi.*(Pt_1-(b0*unos)-mu_J)).^2)./ ...
%     (2.*(sigmaSq+sigmaSq_J))).* (1/sqrt(2.*pi.*(sigmaSq+sigmaSq_J))) + ...
%     (1-lambda).*exp((-(Pt-(b0*unos)-phi.*(Pt_1-(b0*unos))).^2)/(2.*sigmaSq)).* ...
%     (1/sqrt(2.*pi.*sigmaSq));

mrjpdf = @(Pt, phi, sigmaSq, b0) ...
    exp((-(Pt-(b0*unos)-phi.*(Pt_1-(b0*unos))).^2)/(2.*sigmaSq)).*(1/sqrt(2.*pi.*sigmaSq));

% Constraints: 
% phi < 1 (k > 0)
% sigmaSq > 0
% sigmaSq_J > 0
% 0 <= lambda <= 1
% lb = [-Inf -Inf 0 0 0 -Inf*ones(1,28)];
% ub = [1 Inf Inf Inf 1 Inf*ones(1,28)];
lb = [-Inf 0 -Inf];
ub = [1 Inf Inf];

% Initial values
x0 = [0 var(X) 4.53];

% Solve maximum likelihood
params = mle(Pt,'pdf',mrjpdf,'start',x0,'lowerbound',lb,'upperbound',ub,...
    'optimfun','fmincon');

% Obtain calibrated parameters
kappa = (1-params(1))
mu_J = params(2)
sigma = sqrt(params(3))
sigma_J = sqrt(params(4))
lambda = params(5)
b0 = params(6)

%% Monte Carlo Simulation

% Simulate for Test set
nPeriods = length(logPrices_Test)+1;
nTrials = 1000;
n1 = randn(nPeriods,nTrials);
n2 = randn(nPeriods, nTrials);
j = binornd(1, lambda, nPeriods, nTrials);
SimPrices = zeros(nPeriods, nTrials);
SimPrices(1,:) = X(end);
for i=2:nPeriods
    SimPrices(i,:) = SimPrices(i-1,:).*exp(-kappa) + ...
            sigma*sqrt((1-exp(-2*kappa))/(2*kappa)).*n1(i,:)+ j(i,:).*(mu_J+sigma_J*n2(i,:));
end

% Add back seasonality
SimPriceDates = PriceDates_Test;
SimPriceTimes = yearfrac(SimPriceDates(1), SimPriceDates);
CSim = X_TestM8;
logSimPrices = SimPrices(2:end,:) + repmat(CSim*seasonParam,1,nTrials);

% Plot logarithm of Prices and simulated logarithm of Prices
figure;
subplot(2, 1, 1);
plot(PriceDates, logPrices);
hold on;
plot(SimPriceDates, logSimPrices(:,1:5));%, 'red');
seasonLine = [C; CSim]*seasonParam;
plot([PriceDates_Train; SimPriceDates], seasonLine, 'green');
hold off;
datetick();
title('Actual log(price) and Simulated log(price)');
xlabel('Date');
ylabel('log(price)');
legend('market', 'simulation','Location','southeast');

% Plot prices and simulated prices
PricesSim = exp(logSimPrices);
subplot(2, 1, 2);
plot(PriceDates, PricesC);
hold on;
% plot(SimPriceDates, PricesSim(:,:));
plot(SimPriceDates, PricesSim(:,2), 'red');
hold off;
datetick();
title('Actual Prices and Simulated Prices');
xlabel('Date');
ylabel('Price ($)');
legend('market', 'simulation', 'Location',  'northwest');

RMSE=mean(mean(((PricesSim-repmat(exp(logPrices_Test),1,nTrials))).^2,1))
MAD=mean(mean(abs((PricesSim-repmat(exp(logPrices_Test),1,nTrials))),1))
MAPE=mean(mean(abs((PricesSim-repmat(exp(logPrices_Test),1,nTrials))./PricesSim),1))
Errores=[RMSE;MAD;MAPE];

%% Option Pricing Monte Carlo Simulation

ExerPrice=50:50:500;

OptionPrice=zeros(1,length(ExerPrice));
HW=zeros(1,length(ExerPrice));
significance=0.05;
DiscRate=0.1/8760;
% Simulate for Test set
for p=1:length(ExerPrice)
    nPeriods = length(logPrices_Test)+1;
    nTrials = 1000;
    n1 = randn(nPeriods,nTrials);
    n2 = randn(nPeriods, nTrials);
    j = binornd(1, lambda, nPeriods, nTrials);
    SimPrices = zeros(nPeriods, nTrials);
    SimPrices(1,:) = X(end);
    for i=2:nPeriods
        SimPrices(i,:) = SimPrices(i-1,:)*exp(-kappa) + ...
            sigma*((1-exp(-2*kappa))/(2*kappa))*n1(i,:) + j(i,:).*(mu_J+sigma_J*n2(i,:));
    end
    
    % Add back seasonality
    SimPriceDates = PriceDates_Test;
    SimPriceTimes = yearfrac(SimPriceDates(1), SimPriceDates);
    CSim = X_TestM5;
    logSimPrices = SimPrices(2:end,:) + repmat(CSim*seasonParam,1,nTrials);
    PricesSim = exp(logSimPrices);
    
    CashFlow=[zeros(1,nTrials);max(0,PricesSim-ExerPrice(p))];
    PVs=pvvar(CashFlow,DiscRate);
    OptionPrice(p)=mean(PVs);
    HW(p)=(std(PVs)*tinv(1-significance/2,nTrials))/sqrt(nTrials);
end

figure()
errorbar(ExerPrice,OptionPrice,HW,'o')
ylabel('Reliability Charge (COP/MWh)')
xlabel('Scarcity Price (COP/kWh)')