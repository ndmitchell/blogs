target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-f80:128:128-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32"
target triple = "i686-pc-win32"

declare  ccc i8* @memcpy(i8*, i8*, i32)

declare  ccc i8* @memmove(i8*, i8*, i32)

declare  ccc i8* @memset(i8*, i32, i32)

declare  ccc i32 @newSpark(i8*, i8*)

!0 = metadata !{metadata !"top"}
!1 = metadata !{metadata !"stack",metadata !0}
!2 = metadata !{metadata !"heap",metadata !0}
!3 = metadata !{metadata !"rx",metadata !2}
!4 = metadata !{metadata !"base",metadata !0}
!5 = metadata !{metadata !"other",metadata !0}

define  cc 10 void @InnerLoop_zdwgo_info(i32* noalias nocapture %Base_Arg, i32* noalias nocapture %Sp_Arg, i32* noalias nocapture %Hp_Arg, i32 %R1_Arg) align 4 nounwind section "X98A__STRIP,__me2"
{
c1Ue:
  %Base_Var = alloca i32*, i32 1
  store i32* %Base_Arg, i32** %Base_Var
  %Sp_Var = alloca i32*, i32 1
  store i32* %Sp_Arg, i32** %Sp_Var
  %Hp_Var = alloca i32*, i32 1
  store i32* %Hp_Arg, i32** %Hp_Var
  %R1_Var = alloca i32, i32 1
  store i32 %R1_Arg, i32* %R1_Var
  %ls1Sd = alloca i32, i32 1
  %ls1Sg = alloca i32, i32 1
  %ls1Sf = alloca i32, i32 1
  %lc1TY = alloca i32, i32 1
  %ls1Tx = alloca i32, i32 1
  %ls1TC = alloca i32, i32 1
  %ls1TB = alloca i32, i32 1
  %ln1ZM = load i32** %Sp_Var
  %ln1ZN = getelementptr inbounds i32* %ln1ZM, i32 0
  %ln1ZO = bitcast i32* %ln1ZN to i32*
  %ln1ZP = load i32* %ln1ZO, !tbaa !1
  %ln1ZQ = inttoptr i32 %ln1ZP to i8*
  %ln1ZR = load i8* %ln1ZQ, !tbaa !5
  %ln1ZS = zext i8 %ln1ZR to i32
  store i32 %ln1ZS, i32* %ls1Sd
  %ln1ZT = load i32* %ls1Sd
  store i32 %ln1ZT, i32* %ls1Sg
  %ln1ZU = load i32* %ls1Sg
  store i32 %ln1ZU, i32* %ls1Sf
  %ln1ZV = load i32* %ls1Sf
  %ln1ZW = icmp ule i32 %ln1ZV, 36
  %ln1ZX = zext i1 %ln1ZW to i32
  store i32 %ln1ZX, i32* %lc1TY
  %ln1ZY = load i32* %lc1TY
  %ln1ZZ = icmp uge i32 %ln1ZY, 1
  br i1 %ln1ZZ, label %c1Uh, label %n200

n200:
  %ln201 = load i32** %Sp_Var
  %ln202 = getelementptr inbounds i32* %ln201, i32 0
  %ln203 = bitcast i32* %ln202 to i32*
  %ln204 = load i32* %ln203, !tbaa !1
  %ln205 = add i32 %ln204, 1
  store i32 %ln205, i32* %ls1Tx
  %ln206 = load i32* %ls1Tx
  %ln207 = load i32** %Sp_Var
  %ln208 = getelementptr inbounds i32* %ln207, i32 0
  store i32 %ln206, i32* %ln208, !tbaa !1
  %ln209 = load i32** %Base_Var
  %ln20a = load i32** %Sp_Var
  %ln20b = load i32** %Hp_Var
  %ln20c = load i32* %R1_Var
  tail call cc 10 void (i32*,i32*,i32*,i32)* @InnerLoop_zdwgo_info( i32* %ln209, i32* %ln20a, i32* %ln20b, i32 %ln20c ) nounwind
  ret void

c1Uh:
  %ln20d = load i32* %ls1Sf
  store i32 %ln20d, i32* %ls1TC
  %ln20e = load i32* %ls1TC
  %ln20f = icmp ult i32 %ln20e, 13
  br i1 %ln20f, label %c1Uq, label %n20g

n20g:
  %ln20h = load i32* %ls1TC
  %ln20i = icmp ult i32 %ln20h, 32
  br i1 %ln20i, label %c1Ur, label %n20j

n20j:
  %ln20k = load i32* %ls1TC
  %ln20l = icmp ult i32 %ln20k, 36
  br i1 %ln20l, label %c1Us, label %n20m

n20m:
  %ln20n = load i32* %ls1TC
  %ln20o = icmp ne i32 %ln20n, 36
  br i1 %ln20o, label %c1Ut, label %n20p

n20p:
  %ln20q = load i32** %Sp_Var
  %ln20r = getelementptr inbounds i32* %ln20q, i32 0
  %ln20s = bitcast i32* %ln20r to i32*
  %ln20t = load i32* %ln20s, !tbaa !1
  store i32 %ln20t, i32* %R1_Var
  %ln20u = load i32** %Sp_Var
  %ln20v = getelementptr inbounds i32* %ln20u, i32 1
  %ln20w = ptrtoint i32* %ln20v to i32
  %ln20x = inttoptr i32 %ln20w to i32*
  store i32* %ln20x, i32** %Sp_Var
  %ln20y = load i32** %Sp_Var
  %ln20z = getelementptr inbounds i32* %ln20y, i32 0
  %ln20A = bitcast i32* %ln20z to i32*
  %ln20B = load i32* %ln20A, !tbaa !1
  %ln20C = inttoptr i32 %ln20B to void (i32*, i32*, i32*, i32)*
  %ln20D = load i32** %Base_Var
  %ln20E = load i32** %Sp_Var
  %ln20F = load i32** %Hp_Var
  %ln20G = load i32* %R1_Var
  tail call cc 10 void (i32*,i32*,i32*,i32)* %ln20C( i32* %ln20D, i32* %ln20E, i32* %ln20F, i32 %ln20G ) nounwind
  ret void

c1Ut:
  %ln20H = load i32** %Sp_Var
  %ln20I = getelementptr inbounds i32* %ln20H, i32 0
  %ln20J = bitcast i32* %ln20I to i32*
  %ln20K = load i32* %ln20J, !tbaa !1
  %ln20L = add i32 %ln20K, 1
  store i32 %ln20L, i32* %ls1TB
  %ln20M = load i32* %ls1TB
  %ln20N = load i32** %Sp_Var
  %ln20O = getelementptr inbounds i32* %ln20N, i32 0
  store i32 %ln20M, i32* %ln20O, !tbaa !1
  %ln20P = load i32** %Base_Var
  %ln20Q = load i32** %Sp_Var
  %ln20R = load i32** %Hp_Var
  %ln20S = load i32* %R1_Var
  tail call cc 10 void (i32*,i32*,i32*,i32)* @InnerLoop_zdwgo_info( i32* %ln20P, i32* %ln20Q, i32* %ln20R, i32 %ln20S ) nounwind
  ret void

c1Us:
  %ln20T = load i32* %ls1TC
  %ln20U = icmp ne i32 %ln20T, 32
  br i1 %ln20U, label %c1Ut, label %n20V

n20V:
  %ln20W = load i32** %Sp_Var
  %ln20X = getelementptr inbounds i32* %ln20W, i32 0
  %ln20Y = bitcast i32* %ln20X to i32*
  %ln20Z = load i32* %ln20Y, !tbaa !1
  store i32 %ln20Z, i32* %R1_Var
  %ln210 = load i32** %Sp_Var
  %ln211 = getelementptr inbounds i32* %ln210, i32 1
  %ln212 = ptrtoint i32* %ln211 to i32
  %ln213 = inttoptr i32 %ln212 to i32*
  store i32* %ln213, i32** %Sp_Var
  %ln214 = load i32** %Sp_Var
  %ln215 = getelementptr inbounds i32* %ln214, i32 0
  %ln216 = bitcast i32* %ln215 to i32*
  %ln217 = load i32* %ln216, !tbaa !1
  %ln218 = inttoptr i32 %ln217 to void (i32*, i32*, i32*, i32)*
  %ln219 = load i32** %Base_Var
  %ln21a = load i32** %Sp_Var
  %ln21b = load i32** %Hp_Var
  %ln21c = load i32* %R1_Var
  tail call cc 10 void (i32*,i32*,i32*,i32)* %ln218( i32* %ln219, i32* %ln21a, i32* %ln21b, i32 %ln21c ) nounwind
  ret void

c1Ur:
  %ln21d = load i32* %ls1TC
  %ln21e = icmp ne i32 %ln21d, 13
  br i1 %ln21e, label %c1Ut, label %n21f

n21f:
  %ln21g = load i32** %Sp_Var
  %ln21h = getelementptr inbounds i32* %ln21g, i32 0
  %ln21i = bitcast i32* %ln21h to i32*
  %ln21j = load i32* %ln21i, !tbaa !1
  store i32 %ln21j, i32* %R1_Var
  %ln21k = load i32** %Sp_Var
  %ln21l = getelementptr inbounds i32* %ln21k, i32 1
  %ln21m = ptrtoint i32* %ln21l to i32
  %ln21n = inttoptr i32 %ln21m to i32*
  store i32* %ln21n, i32** %Sp_Var
  %ln21o = load i32** %Sp_Var
  %ln21p = getelementptr inbounds i32* %ln21o, i32 0
  %ln21q = bitcast i32* %ln21p to i32*
  %ln21r = load i32* %ln21q, !tbaa !1
  %ln21s = inttoptr i32 %ln21r to void (i32*, i32*, i32*, i32)*
  %ln21t = load i32** %Base_Var
  %ln21u = load i32** %Sp_Var
  %ln21v = load i32** %Hp_Var
  %ln21w = load i32* %R1_Var
  tail call cc 10 void (i32*,i32*,i32*,i32)* %ln21s( i32* %ln21t, i32* %ln21u, i32* %ln21v, i32 %ln21w ) nounwind
  ret void

c1Uu:
  %ln21x = load i32* %ls1TC
  %ln21y = icmp ne i32 %ln21x, 0
  br i1 %ln21y, label %c1Ut, label %n21z

n21z:
  %ln21A = load i32** %Sp_Var
  %ln21B = getelementptr inbounds i32* %ln21A, i32 0
  %ln21C = bitcast i32* %ln21B to i32*
  %ln21D = load i32* %ln21C, !tbaa !1
  store i32 %ln21D, i32* %R1_Var
  %ln21E = load i32** %Sp_Var
  %ln21F = getelementptr inbounds i32* %ln21E, i32 1
  %ln21G = ptrtoint i32* %ln21F to i32
  %ln21H = inttoptr i32 %ln21G to i32*
  store i32* %ln21H, i32** %Sp_Var
  %ln21I = load i32** %Sp_Var
  %ln21J = getelementptr inbounds i32* %ln21I, i32 0
  %ln21K = bitcast i32* %ln21J to i32*
  %ln21L = load i32* %ln21K, !tbaa !1
  %ln21M = inttoptr i32 %ln21L to void (i32*, i32*, i32*, i32)*
  %ln21N = load i32** %Base_Var
  %ln21O = load i32** %Sp_Var
  %ln21P = load i32** %Hp_Var
  %ln21Q = load i32* %R1_Var
  tail call cc 10 void (i32*,i32*,i32*,i32)* %ln21M( i32* %ln21N, i32* %ln21O, i32* %ln21P, i32 %ln21Q ) nounwind
  ret void

c1Uq:
  %ln21R = load i32* %ls1TC
  %ln21S = icmp ult i32 %ln21R, 10
  br i1 %ln21S, label %c1Uu, label %n21T

n21T:
  %ln21U = load i32* %ls1TC
  %ln21V = icmp ne i32 %ln21U, 10
  br i1 %ln21V, label %c1Ut, label %n21W

n21W:
  %ln21X = load i32** %Sp_Var
  %ln21Y = getelementptr inbounds i32* %ln21X, i32 0
  %ln21Z = bitcast i32* %ln21Y to i32*
  %ln220 = load i32* %ln21Z, !tbaa !1
  store i32 %ln220, i32* %R1_Var
  %ln221 = load i32** %Sp_Var
  %ln222 = getelementptr inbounds i32* %ln221, i32 1
  %ln223 = ptrtoint i32* %ln222 to i32
  %ln224 = inttoptr i32 %ln223 to i32*
  store i32* %ln224, i32** %Sp_Var
  %ln225 = load i32** %Sp_Var
  %ln226 = getelementptr inbounds i32* %ln225, i32 0
  %ln227 = bitcast i32* %ln226 to i32*
  %ln228 = load i32* %ln227, !tbaa !1
  %ln229 = inttoptr i32 %ln228 to void (i32*, i32*, i32*, i32)*
  %ln22a = load i32** %Base_Var
  %ln22b = load i32** %Sp_Var
  %ln22c = load i32** %Hp_Var
  %ln22d = load i32* %R1_Var
  tail call cc 10 void (i32*,i32*,i32*,i32)* %ln229( i32* %ln22a, i32* %ln22b, i32* %ln22c, i32 %ln22d ) nounwind
  ret void

}
