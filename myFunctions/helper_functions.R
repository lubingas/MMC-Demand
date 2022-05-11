### Create random parameters
#####################################################################
# code for MLHS							    #
#####################################################################

shuffle=function(inv){
  out=inv[rank(runif(length(inv)))];
  out}

mlhs=function(N,d,i){
  temp=seq(0,N-1)/N;
  out=matrix(0,N*i,d);
  j=1;
  k=1;
  while(j<i+1){
    k=1;
    while(k<d+1){
      out[(1+N*(j-1)):(N*j),k]=shuffle(temp+runif(1)/N);
      k=k+1}
    j=j+1}
  out}


randCoeff = function(beta, draws) {
  # needed to be able to refer to parameters by name
  beta1=as.list(beta)
  draws1=as.list(draws)
  attach(beta1)
  attach(draws1)
  
  randcoeff = list()
  
  randcoeff[['neither']]      =      neither_mu             + s0101*eta01
  randcoeff[['permhc']]       =      permhc_mu              + s0201*eta01 + s0202*eta02
  randcoeff[['dist5']]        =      dist5_mu               + s0301*eta01 + s0302*eta02 + s0303*eta03
  randcoeff[['dist15']]       =      dist15_mu              + s0401*eta01 + s0402*eta02 + s0403*eta03 + s0404*eta04
  randcoeff[['avail']]        =      avail_mu               + s0501*eta01 + s0502*eta02 + s0503*eta03 + s0504*eta04 + s0505*eta05
  randcoeff[['somepriv']]     =      somepriv_mu            + s0601*eta01 + s0602*eta02 + s0603*eta03 + s0604*eta04 + s0605*eta05 + s0606*eta06
  randcoeff[['notpriv']]      =      notpriv_mu             + s0701*eta01 + s0702*eta02 + s0703*eta03 + s0704*eta04 + s0705*eta05 + s0706*eta06 + s0707*eta07
  randcoeff[['device']]       =      device_mu              + s0801*eta01 + s0802*eta02 + s0803*eta03 + s0804*eta04 + s0805*eta05 + s0806*eta06 + s0807*eta07 + s0808*eta08
  randcoeff[['voucher']]      =      voucher_mu             + s0901*eta01 + s0902*eta02 + s0903*eta03 + s0904*eta04 + s0905*eta05 + s0906*eta06 + s0907*eta07 + s0908*eta08 + s0909*eta09
  randcoeff[['cash']]         =      cash_mu                + s1001*eta01 + s1002*eta02 + s1003*eta03 + s1004*eta04 + s1005*eta05 + s1006*eta06 + s1007*eta07 + s1008*eta08 + s1009*eta09 + s1010*eta10
  randcoeff[['pricenone']]    =      pricenone_mu           + s1101*eta01 + s1102*eta02 + s1103*eta03 + s1104*eta04 + s1105*eta05 + s1106*eta06 + s1107*eta07 + s1108*eta08 + s1109*eta09 + s1110*eta10 + s1111*eta11
  randcoeff[['pricevoucher']] =      pricevoucher_mu        + s1201*eta01 + s1202*eta02 + s1203*eta03 + s1204*eta04 + s1205*eta05 + s1206*eta06 + s1207*eta07 + s1208*eta08 + s1209*eta09 + s1210*eta10 + s1211*eta11 + s1212*eta12
  randcoeff[['pricecash']]    =      pricecash_mu           + s1301*eta01 + s1302*eta02 + s1303*eta03 + s1304*eta04 + s1305*eta05 + s1306*eta06 + s1307*eta07 + s1308*eta08 + s1309*eta09 + s1310*eta10 + s1311*eta11 + s1312*eta12 + s1313*eta13
  detach(beta1)
  detach(draws1)
  return(randcoeff)
}
