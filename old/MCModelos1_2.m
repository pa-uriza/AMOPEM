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
Prices_Train=PricesC((PriceDates>=fromTrain)&(PriceDates<=toTrain),:);
X_TrainM5=X_M5((PriceDates>=fromTrain)&(PriceDates<=toTrain),:);
X_TrainM6=X_M6((PriceDates>=fromTrain)&(PriceDates<=toTrain),:);
X_TrainM7=X_M7((PriceDates>=fromTrain)&(PriceDates<=toTrain),:);
X_TrainM8=X_M8((PriceDates>=fromTrain)&(PriceDates<=toTrain),:);

% Create Test set
Prices_Test=PricesC((PriceDates>=fromTest)&(PriceDates<=toTest),:);
X_TestM5=X_M5((PriceDates>=fromTest)&(PriceDates<=toTest),:);
X_TestM6=X_M6((PriceDates>=fromTest)&(PriceDates<=toTest),:);
X_TestM7=X_M7((PriceDates>=fromTest)&(PriceDates<=toTest),:);
X_TestM8=X_M8((PriceDates>=fromTest)&(PriceDates<=toTest),:);

%Params Models 1 to 4 
load('ResModelsR.mat');
%% Monte Carlo Simulation

seasonParam = seasonParamM2;
kappa = kappaM2;
sigma = sigmaM2;
C = X_TrainM6;
CSim = X_TestM6;

X = Prices_Train-C*seasonParam;
% Simulate for Test set
nPeriods = length(logPrices_Test)+1;
nTrials = 1000;
n1 = randn(nPeriods,nTrials);
SimPrices = zeros(nPeriods, nTrials);
SimPrices(1,:) = X(end);
for i=2:nPeriods
    SimPrices(i,:) = SimPrices(i-1,:).*exp(-kappa) + ...
            sigma*sqrt((1-exp(-2*kappa))/(2*kappa)).*n1(i,:);
end

% Add back seasonality
SimPriceDates = PriceDates_Test;
SimPriceTimes = yearfrac(SimPriceDates(1), SimPriceDates);
PricesSim = SimPrices(2:end,:) + repmat(CSim*seasonParam,1,nTrials);

% Plot prices and simulated prices
figure;
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

RMSE=mean(sqrt(mean(((PricesSim-repmat(Prices_Test,1,nTrials))).^2,1)))
MAD=mean(mean(abs((PricesSim-repmat(Prices_Test,1,nTrials))),1))
MAPE=mean(mean(abs((PricesSim-repmat(Prices_Test,1,nTrials))./repmat(Prices_Test,1,nTrials)),1))
Errores=[RMSE;MAD;MAPE];

%% Option Pricing Monte Carlo Simulation

ExerPrice=50:50:500;

OptionPrice=zeros(1,length(ExerPrice));
HW=zeros(1,length(ExerPrice));
significance=0.05;
ARate=0.1;
% Simulate for Test set
for p=1:length(ExerPrice)
    nPeriods = length(logPrices_Test)+1;
    nTrials = 1000;
    n1 = randn(nPeriods,nTrials);
    SimPrices = zeros(nPeriods, nTrials);
    SimPrices(1,:) = X(end);
    for i=2:nPeriods
        SimPrices(i,:) = SimPrices(i-1,:)*exp(-kappa) + ...
            sigma*((1-exp(-2*kappa))/(2*kappa))*n1(i,:);
    end
    
    % Add back seasonality
    SimPriceDates = PriceDates_Test;
    SimPriceTimes = yearfrac(SimPriceDates(1), SimPriceDates);
    PricesSim = SimPrices(2:end,:) + repmat(CSim*seasonParam,1,nTrials);
        
    CashFlow=max(0,PricesSim-ExerPrice(p));
    PVs=sum(repmat(exp(-ARate*(1:8760)/8760)',1,nTrials).*CashFlow,2)/8760;
    OptionPrice(p)=mean(PVs);
    HW(p)=(std(PVs)*tinv(1-significance/2,nTrials))/sqrt(nTrials);
end

figure()
errorbar(ExerPrice,OptionPrice,HW,'o')
ylabel('Reliability Charge (COP/MWh)')
xlabel('Scarcity Price (COP/kWh)')