function fseminf_ip(varargin)

//Ensuring a variable number of input arguments.
    [lhs , rhs] = argn();

    fun = varargin(1);
    x0 = varargin(2);
    ntheta = varargin(3);
    seminfcon = varargin(4);
    A = [];
    b = [];
    Aeq = [];
    beq = [];
    lb = [];
    ub = [];
    if ( rhs<4 | rhs>11 ) then
            errmsg = msprintf(gettext("%s: Unexpected number of input arguments : %d provided while it should be 4,6,8,9,10"), "fseminf_ip", rhs);
            error(errmsg)
    end
        
    if (rhs==5 | rhs==7 | rhs ==9) then
        errmsg = msprintf(gettext("%s: Unexpected number of input arguments : %d provided while it should be 4,6,8,10,11"), "fseminf_ip", rhs);
        error(errmsg)
    end

    if (rhs>4) then
        A = varargin(5);
        B = varargin(6);
    end

    if (rhs>6) then
        Aeq = varargin(7);
        beq = varargin(8);
    end

    if (rhs>8) then
        lb  = varargin(9);
        ub  = varargin(10);
    end
    //To check whether the 1st Input argument (fun) is a function or not
    Checktype("fseminf_ip", fun, "f", 1, "function");

    //To check whether the 2nd Input argument (x0) is a vector/scalar
    Checktype("fseminf_ip", x0, "x0", 2, "constant");

    //To check and convert the 2nd Input argument (x0) to a row vector 
    if((size(x0,1)~=1) & (size(x0,2)~=1)) then
        errmsg = msprintf(gettext("%s: Expected Row Vector or Column Vector for x0 (Starting Point) or Starting Point cannot be Empty"), "fseminf_ip");
        error(errmsg);
    end

    if(size(x0,2)==1) then
        x0=x0';     //Converting x0 to a row vector, if it is a column vector
    else 
        x0=x0;      //Retaining the same, if it is already a row vector
    end         
    
    s=size(x0);

    //To check the match between fun (1st Parameter) and x0 (2nd Parameter)

    if(execstr('init=fun(x0)','errcatch')==21) then
        errmsg = msprintf(gettext("%s: Objective function and x0 did not match"), "fseminf_ip");
        error(errmsg);
    end

    function [y,check] = f(x)
        if(execstr('y=fun(x)','errcatch')==32 | execstr('y=fun(x)','errcatch')==27)
            y=0;
            check=1;
        else
            y=fun(x);
            if (isreal(y)==%F) then
                y=0;
                check=1;
            else
                check=0;
            end
        end
    endfunction

    Checktype("fseminf_ip", ntheta, "ntheta", 3, "constant");


    //To check whether the 3rd input, ntheta, is a constant or not.
    if((size(ntheta,1)~=1) | (size(ntheta,2)~=1)) then
        disp("first error test cleared")
        errmsg = msprintf(gettext("%s: Expected single numeric value for ntheta (No. of semi infinite constraints."), "fseminf_ip");
        error(errmsg);
    end
    //To check whether the 4th input seminfcon is a function
    disp("We enter seminfcon")
    if (type(seminfcon) == 1 & size(seminfcon,2)==0 ) then
        addsemi_ic=[];
        addsemi_ic1=[];
        no_semi_ic=0;
        no_semi_iic=0;
        no_semi_iec=0;
        disp("First Condition execd")
    elseif (type(seminfcon) == 13 | type(seminfcon) == 11) 
        
        if(execstr('[sample_c,sample_ceq,K,S] = seminfcon(x0,S)','errcatch')==21)
            errmsg = msprintf(gettext("%s: The semi infinitely Constrained function(4th Parameter) and the inputs (x0(2nd Parameter) and S (sample space)) did not match"), "fseminf_ip");
            error(errmsg);  
        end 
        disp("First sanity check")

        [sample_c,sample_ceq,sample_K,sample_S] = seminfcon(x0,S);

        if (size(sample_c,1)~=1 & size(sample_c,1)~=0) then
            errmsg = msprintf(gettext("%s: Definition of c in Semi infinite Constraint function(4th Parameter) should be in the form of Row Vector or Empty Vector"), "fseminf_ip");
            error(errmsg)
        end
        
        if (size(sample_ceq,1)~=1 & size(sample_ceq,1)~=0) then
            errmsg = msprintf(gettext("%s: Definition of ceq in a Semi infinitely Constrained function(4th Parameter) should be in the form of Row Vector or Empty Vector"), "fseminf_ip");
            error(errmsg)
        end

        if (size(sample_S,2)~=2 ) then
            errmsg = msprintf(gettext("%s: Definition of the sample space in a Semi infinitely Constrained function(4th Parameter) should have two columns"), "fseminf_ip");
            error(errmsg)
        end

        if (size(sample_K,1)~=1 ) then
            errmsg = msprintf(gettext("%s: Definition of the K in a Semi infinitely Constrained function(4th Parameter) should have one column."), "fseminf_ip");
            error(errmsg)
        end
        disp("Combining constraints")

        no_nlic = size(sample_c,2);
        no_nlec = size(sample_ceq,2);
        no_k = size(sample_K,1);

        no_nlc = no_nlic + no_nlec + no_k;

        function y = addnlc(x,S)
            [c,ceq,K,S] = seminfcon(x);
            y = [c';ceq';K'];
        endfunction

        if(execstr('sample_allcon = addnlc(x0,S)','errcatch')==21)
            errmsg = msprintf(gettext("%s: Semi infinite Constraint function(4th Parameter) and x0(2nd Parameter) did not match"), "fseminf_ip");
            error(errmsg);
        end
        sample_allcon = addnlc(x0,S);
        disp(sample_allcon)
        disp(no_n)
        if (size(sample_allcon,1)==0 & size(sample_allcon,2)==0) 
            continue
        
        elseif (size(sample_allcon,1)~=no_nlc | size(sample_allcon,2)~=1) then
            errmsg = msprintf(gettext("%s: Please check the Semi infinite Constraint function (4th Parameter) function"), "fseminf_ip");
            error(errmsg)
        end
        disp("Second sanity check")
        //Constructing a nlc function with error deduction
        function [y,check] = addnlc1(x,S)
            if(execstr('y = addnlc(x,S)','errcatch')==32 | execstr('y = addnlc(x,S)','errcatch')==27)
                y = 0;
                check=1;
            else
                y = addnlc(x,S);
                if (isreal(y)==%F) then
                    y = 0;
                    check=1;
                else
                    check=0;
                end
            end
        endfunction
        
    else 
        errmsg = msprintf(gettext("%s: Semi infinite Constraint (4th Parameter) should be a function or an Empty Matrix"), "fseminf_ip");
        error(errmsg)
    end


    //To check whether the 5th Input argument (A) is a Matrix/Vector
    Checktype("fseminf_ip", A, "A", 5, "constant");

    //To check for correct size of A(4thrd paramter)
    if(size(A,2)~=s(2) & size(A,2)~=0) then
        errmsg = msprintf(gettext("%s: Expected Matrix of size (No of linear inequality constraints X No of Variables) or an Empty Matrix for Linear Inequality Constraint coefficient Matrix A"), "fseminf_ip");
        error(errmsg);
    end

    s1=size(A);
    disp("Check A")

    
    //To check whether the 6th Input argument (b) is a vector/scalar
    Checktype("fseminf_ip", b, "b", 6, "constant");
    
    //To check for the correct size of b (6th paramter) and convert it into a column vector which is required for Ipopt
    if(s1(2)==0) then
        if(size(b,2)~=0) then
            errmsg = msprintf(gettext("%s: As Linear Inequality Constraint coefficient Matrix A (3rd parameter) is empty, b (4th Parameter) should also be empty"), "fseminf_ip");
            error(errmsg);
        end
    else
        if((size(b,1)~=1) & (size(b,2)~=1)) then
            errmsg = msprintf(gettext("%s: Expected Non empty Row/Column Vector for b (4th Parameter) for your Inputs "), "fseminf_ip");
            error(errmsg);
        elseif(size(b,1)~=s1(1) & size(b,2)==1) then
            errmsg = msprintf(gettext("%s: Expected Column Vector (number of linear inequality constraints X 1) for b (4th Parameter) "), "fseminf_ip");
            error(errmsg);
        elseif(size(b,1)==s1(1) & size(b,2)==1) then 
            b=b;
        elseif(size(b,1)==1 & size(b,2)~=s1(1)) then
            errmsg = msprintf(gettext("%s: Expected Row Vector (1 X number of linear inequality constraints) for b (4th Parameter) "), "fseminf_ip");
            error(errmsg);
        elseif(size(b,1)==1 & size(b,2)==s1(1)) then
            b=b';
        end 
    end
    disp("Check B")

    //To check whether the 7th Input argument (Aeq) is a matrix/vector
    Checktype("fseminf_ip", Aeq, "Aeq", 7, "constant");
    
    //To check for the correct size of Aeq (7th paramter)
    if(size(Aeq,2)~=s(2) & size(Aeq,2)~=0) then
        errmsg = msprintf(gettext("%s: Expected Matrix of size (No of linear equality constraints X No of Variables) or an Empty Matrix for Linear Equality Constraint coefficient Matrix Aeq"), "fseminf_ip");
        error(errmsg);
    end

    s2=size(Aeq);
    disp("Check Aeq")

    //To check whether the 8th Input argument(beq) is a vector/scalar
    Checktype("fseminf_ip", beq, "beq", 8, "constant");
    
    //To check for the correct size of beq(8th paramter) and convert it into a column vector which is required for Ipopt
    if(s2(2)==0) then
        if(size(beq,2)~=0) then
            errmsg = msprintf(gettext("%s: As Linear Equality Constraint coefficient Matrix Aeq (7th parameter) is empty, beq (6th Parameter) should also be empty"), "fseminf_ip");
            error(errmsg);
        end
    else
        if((size(beq,1)~=1) & (size(beq,2)~=1)) then
            errmsg = msprintf(gettext("%s: Expected Non empty Row/Column Vector for beq (8th Parameter)"), "fseminf_ip");
            error(errmsg);
        elseif(size(beq,1)~=s2(1) & size(beq,2)==1) then
            errmsg = msprintf(gettext("%s: Expected Column Vector (number of linear equality constraints X 1) for beq (8th Parameter) "), "fseminf_ip");
            error(errmsg);
        elseif(size(beq,1)==s2(1) & size(beq,2)==1) then 
            beq=beq;
        elseif(size(beq,1)==1 & size(beq,2)~=s2(1)) then
            errmsg = msprintf(gettext("%s: Expected Row Vector (1 X number of linear equality constraints) for beq (8th Parameter) "), "fseminf_ip");
            error(errmsg);
        elseif(size(beq,1)==1 & size(beq,2)==s2(1)) then
            beq=beq';
        end 
    end
    disp("Check beq")
    
    //To check whether the 9th Input argument (lb) is a vector/scalar
    Checktype("fseminf_ip", lb, "lb", 9, "constant");
    
    //To check for the correct size and data of lb (9th paramter) and convert it into a column vector as required by Ipopt
    if (size(lb,2)==0) then
            lb = repmat(-%inf,1,s(2));
    end
    
    if (size(lb,1)~=1) & (size(lb,2)~=1) then
      errmsg = msprintf(gettext("%s: Lower Bound (7th Parameter) should be a vector"), "fseminf_ip");
      error(errmsg); 
    elseif(size(lb,1)~=s(2) & size(lb,2)==1) then
        errmsg = msprintf(gettext("%s: Expected Column Vector (number of Variables X 1) for lower bound (9th Parameter) "), "fseminf_ip");
        error(errmsg);
    elseif(size(lb,1)==s(2) & size(lb,2)==1) then
        lb=lb;
    elseif(size(lb,1)==1 & size(lb,2)~=s(2)) then
        errmsg = msprintf(gettext("%s: Expected Row Vector (1 X number of Variables) for lower bound (9th Parameter) "), "fseminf_ip");
        error(errmsg);
    elseif(size(lb,1)==1 & size(lb,2)==s(2)) then
        lb=lb';
    end 
    disp("Check lb")

    //To check whether the 10th Input argument (ub) is a vector/scalar
    Checktype("fseminf_ip", ub, "ub", 10, "constant");
    
    //To check for the correct size and data of ub (10th paramter) and convert it into a column vector as required by Ipopt
    if (size(ub,2)==0) then
        ub = repmat(%inf,1,s(2));
    end
    
    if (size(ub,1)~=1)& (size(ub,2)~=1) then
      errmsg = msprintf(gettext("%s: Upper Bound (10th Parameter) should be a vector"), "fseminf_ip");
      error(errmsg); 
    elseif(size(ub,1)~=s(2) & size(ub,2)==1) then
        errmsg = msprintf(gettext("%s: Expected Column Vector (number of Variables X 1) for upper bound (8th Parameter) "), "fseminf_ip");
        error(errmsg);
    elseif(size(ub,1)==s(2) & size(ub,2)==1) then
        ub=ub;
    elseif(size(ub,1)==1 & size(ub,2)~=s(2)) then
        errmsg = msprintf(gettext("%s: Expected Row Vector (1 X number of Variables) for upper bound (10th Parameter) "), "fseminf_ip");
        error(errmsg);
    elseif(size(ub,1)==1 & size(ub,2)==s(2)) then
        ub=ub';
    end 
    disp("Check ub")

    //To check the contents of lb & ub (9th & 10th Parameter)
    for i = 1:s(2)
        if (lb(i) == %inf) then
            errmsg = msprintf(gettext("%s: Value of Lower Bound can not be infinity"), "fseminf_ip");
            error(errmsg); 
        end 

        if (ub(i) == -%inf) then
            errmsg = msprintf(gettext("%s: Value of Upper Bound can not be negative infinity"), "fseminf_ip");
            error(errmsg); 
        end 

        if(ub(i)-lb(i)<=1e-6) then
            errmsg = msprintf(gettext("%s: Difference between Upper Bound and Lower bound should be atleast > 10^-6 for variable number= %d "), "fseminf_ip", i);
            error(errmsg)
        end
    end

    disp("return 0, baby")











    

endfunction