function [c, ceq, K, s] = mycon(x,s)

//No finite nonlinear inequality and equality constraints
c = [];
ceq = [];

// Sample set
if isnan(s)
    % Initial sampling interval
    s = [0.01 0];
end
t = 0:s(1):1;

// Evaluate the semi-infinite constraint
K = (x - 0.5) - (t - 0.5).^2;
endfunction