function L = mrjpdf_M5(x)
global C
global Pt
global Pt_1

Cc=C(2:end,:);
Cc_1=C(1:end-1,:);
phi=x(1);mu_J=x(2);sigma=0.01;sigma_J=x(4);lambda=x(5);
seasonParams=x(6:33)';

Xt=Pt-Cc*seasonParams;
Xt_1=Pt_1-Cc_1*seasonParams;

L=-sum(log(lambda*normpdf(Xt,phi*Xt_1+mu_J,sigma+sigma_J)+(1-lambda)*normpdf(Xt,phi*Xt_1,sigma)));
% L=-sum(log(lambda*exp((-(Xt-phi*Xt_1-mu_J).^2)/(2*(sigmaSq+sigmaSq_J)))* (1/sqrt(2*pi*(sigmaSq+sigmaSq_J))) + ...
%     (1-lambda)*exp((-(Xt-phi*Xt_1).^2)/(2*sigmaSq))*(1/sqrt(2*pi*sigmaSq))));
end