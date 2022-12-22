! Convert FIA equation (CU****** or BD******) to NVEL equation format (10 character string)
! The VOLTYPE is the FIA equation volume type:
! CVTS - from ground to tip
! CVT  - from stump to tip
! CV4  - from stump to 4 inch top
! CVS  - from stump to sawlog top diameter
! TIP  - from MTOPP to tip
! SV   - Scribner boardfoot volume
! IV   - International boardfoot volume
! CV*, SV* AND IV* will need MTOPP for the *
! GEOSUB is the variable used for Alaska equation for Afognak Island (input as 'AFO')
!        also Clark equation geocode (1 to 7) and Lasher EQ geocode (01 to 32)
      SUBROUTINE FIAEQ2NVELEQ(FIAEQ,SPN,GEOSUB,MTOPP,NVELEQ,VOLTYPE,
     & ERRFLG)
      CHARACTER*10 FIAEQ,NVELEQ,VOLTYPE,GEOSUB
      INTEGER SPN,ERRFLG,I,J,FIRST,LAST,DONE,ITOP
      REAL MTOPP
      CHARACTER*10 CULIST(361,3),BDLIST(231,3)
      CHARACTER*4 SPC
      CHARACTER*2 TOPDC
      DATA ((BDLIST(J,I), I=1,3), J=1,231) /
!     & 'BD000006', '101DVEW***', 'SV6',
     & 'BD000006', 'R02ALN0***', 'SV6',
     & 'BD000007', 'R02ALN1***', 'SV6',
     & 'BD000008', '102DVEW***', 'SV6',
     & 'BD000009', '101DVEW***', 'SV6',
     & 'BD000011', 'P02WEN0***', 'SV4',
     & 'BD000012', 'P02WEN0***', 'SV6',
     & 'BD000013', 'P02WEN0***', 'SV8',
     & 'BD000014', 'P02WEN0***', 'SV4',
     & 'BD000015', 'P02WEN0***', 'SV6',
     & 'BD000016', 'P02WEN0***', 'SV8',
     & 'BD000017', 'P02WEN1***', 'SV5',
     & 'BD000018', 'P02WEN1***', 'SV6',
     & 'BD000019', 'P02WEN1***', 'SV7',
     & 'BD000020', 'P02WEN1***', 'SV8',
     & 'BD000021', 'P02WEN1***', 'SV5',
     & 'BD000022', 'P02WEN1***', 'SV6',
     & 'BD000023', 'P02WEN1***', 'SV7',
     & 'BD000024', 'P02WEN1***', 'SV8',
     & 'BD000025', 'P02BIG0***', 'SV6',
     & 'BD000026', 'P02BIG0***', 'SV8',
     & 'BD000030', '8**DVEE***', 'IV7/9',
     & 'BD000031', '901DVEE***', 'SV7/9',
     & 'BD000032', '902DVEE***', 'IV7/9',
     & 'BD000033', '903DVEE***', 'IV7/9',
     & 'BD000034', '904DVEE***', 'IV7/9',
     & 'BD000035', '905DVEE***', 'IV7/9',
     & 'BD000036', '906DVEE***', 'IV7/9',
     & 'BD000038', '500DVEW***', 'SV6/8',
     & 'BD000039', '500DVEW***', 'IV6/8',
     & 'BD000040', 'H00SN2W***', 'SV6',
     & 'BD000041', 'N02SCT0***', 'IV6',
     & 'BD000042', 'N01STN0***', 'IV*',
     & 'BD000043', 'N01HAH0***', 'IV6',
     & 'BD000044', 'R00RMR0***', 'IV6',
     & 'BD000045', 'R00RMR0***', 'SV6',
     & 'BD000046', 'R01KEM0***', 'SV6',
     & 'BD000047', 'R01KEM0***', 'IV6',
     & 'BD000048', '102DVEW***', 'SV6',
     & 'BD000049', 'S00SRS0***', 'IV6',
     & 'BD000050', 'P16BRC2***', 'SV616',
     & 'BD000051', 'P32BRC2***', 'SV632',
     & 'BD000052', 'P16BRC2***', 'SV816',
     & 'BD000053', 'P16BRC2***', 'IV6',
     & 'BD000054', 'P16BRC2***', 'IV8',
     & 'BD000055', 'P16BRC1***', 'SV616',
     & 'BD000056', 'P32BRC1***', 'SV632',
     & 'BD000057', 'P16BRC1***', 'SV816',
     & 'BD000058', 'P16BRC1***', 'IV6',
     & 'BD000059', 'P16BRC1***', 'IV8',
     & 'BD000060', 'P16BRI0***', 'SV616',
     & 'BD000061', 'P32BRI0***', 'SV632',
     & 'BD000062', 'P16BRI0***', 'SV816',
     & 'BD000063', 'P16BRI0***', 'IV6',
     & 'BD000064', 'P16BRI0***', 'IV8',
     & 'BD000065', 'P16BRO0***', 'SV616',
     & 'BD000066', 'P32BRO0***', 'SV632',
     & 'BD000067', 'P16BRO0***', 'SV816',
     & 'BD000068', 'P16BRO0***', 'IV6',
     & 'BD000069', 'P16BRO0***', 'IV8',
     & 'BD000070', 'P16SMF0***', 'SV616',
     & 'BD000071', 'P32SMF0***', 'SV632',
     & 'BD000072', 'P16SMF0***', 'SV816',
     & 'BD000073', 'P16SMF0***', 'IV6',
     & 'BD000074', 'P16SMF0***', 'IV8',
     & 'BD000075', 'P16PIL0***', 'SV616',
     & 'BD000076', 'P32PIL0***', 'SV632',
     & 'BD000077', 'P16PIL0***', 'SV816',
     & 'BD000078', 'P16PIL0***', 'IV6',
     & 'BD000079', 'P16PIL0***', 'IV8',
     & 'BD000080', 'P16KRU1***', 'SV6',
     & 'BD000081', 'P16KRU0***', 'SV616',
     & 'BD000082', 'P32KRU0***', 'SV632',
     & 'BD000083', 'P16KRU0***', 'SV816',
     & 'BD000084', 'P16KRU0***', 'IV6',
     & 'BD000085', 'P16KRU0***', 'IV8',
     & 'BD000086', 'A00DVEW***', 'SV6',
     & 'BD000087', 'A00DVEW***', 'IV6',
     & 'BD000088', 'A01DVEW***', 'IV6',
     & 'BD000089', 'A01DVEW***', 'SV6',
     & 'BD000090', 'P01DEM0***', 'SV6',
     & 'BD000091', 'P01DEM0***', 'IV6',
     & 'BD000092', 'P01EMB0***', 'SV6',
     & 'BD000093', 'P01EMB0***', 'IV6',
     & 'BD000095', 'P01BDE0***', 'IV6',
     & 'BD000096', 'P02MAC1***', 'SV6',
     & 'BD000097', 'P02MAC1***', 'IV6',
     & 'BD000098', 'P16MAC1***', 'SV616',
     & 'BD000099', 'P32MAC1***', 'SV632',
     & 'BD000100', 'P16MAC1***', 'SV816',
     & 'BD000101', 'P16MAC1***', 'IV6',
     & 'BD000102', 'P16MAC1***', 'IV8',
     & 'BD015001', '301DVEW015', 'SV6',
     & 'BD015002', '302DVEW015', 'SV6',
     & 'BD015003', 'P02MAC1015', 'IV6',
     & 'BD015004', 'P02MAC1015', 'SV6',
     & 'BD015005', 'P16MAC0015', 'SV616',
     & 'BD015006', 'P32MAC0015', 'SV632',
     & 'BD015007', 'P16MAC0015', 'SV816',
     & 'BD015008', 'P16MAC0015', 'IV6',
     & 'BD015009', 'P16MAC0015', 'IV8',
     & 'BD015010', '301DVEW015', 'IV6',
     & 'BD015011', '302DVEW015', 'IV6',
     & 'BD020001', 'P02MAC1020', 'IV6',
     & 'BD020002', 'P02MAC1020', 'SV6',
     & 'BD020003', 'P16MAC0020', 'SV616',
     & 'BD020004', 'P32MAC0020', 'SV632',
     & 'BD020005', 'P16MAC0020', 'SV816',
     & 'BD020006', 'P16MAC0020', 'IV6',
     & 'BD020007', 'P16MAC0020', 'IV8',
     & 'BD042002', 'P01BDE0042', 'IV6',
     & 'BD064001', 'P16CHT0064', 'SV616',
     & 'BD064002', 'P32CHT0064', 'SV632',
     & 'BD064003', 'P16CHT0064', 'SV816',
     & 'BD064004', 'P16CHT0064', 'IV6',
     & 'BD064005', 'P16CHT0064', 'IV8',
     & 'BD081001', 'P02MAC1081', 'IV6',
     & 'BD081002', 'P02MAC1081', 'SV6',
     & 'BD081003', 'P16MAC0081', 'SV616',
     & 'BD081004', 'P32MAC0081', 'SV632',
     & 'BD081005', 'P16MAC0081', 'SV816',
     & 'BD081006', 'P16MAC0081', 'IV6',
     & 'BD081007', 'P16MAC0081', 'IV8',
     & 'BD093001', '200DVEW093', 'SV6',
     & 'BD093002', '300DVEW093', 'SV6',
     & 'BD093003', '200DVEW093', 'IV6',
     & 'BD093004', '300DVEW093', 'IV6',
     & 'BD094001', 'P01DIP0094', 'SV6',
     & 'BD094002', 'A00DVEW094', 'SV6',
     & 'BD108001', '200DVEW108', 'SV6',
     & 'BD108002', '103DVEW108', 'SV6',
     & 'BD108003', '104DVEW108', 'SV6',
     & 'BD108004', 'P02MAC1108', 'IV6',
     & 'BD108005', 'P02MAC1108', 'SV6',
     & 'BD108006', '200DVEW108', 'SV8',
     & 'BD108007', '200DVEW108', 'IV8',
     & 'BD108008', 'P16MAC0108', 'SV616',
     & 'BD108009', 'P32MAC0108', 'SV632',
     & 'BD108010', 'P16MAC0108', 'SV816',
     & 'BD108011', 'P16MAC0108', 'IV6',
     & 'BD108012', 'P16MAC0108', 'IV8',
     & 'BD113001', '300DVEW113', 'SV6',
     & 'BD113002', '300DVEW113', 'IV6',
     & 'BD117001', 'P02MAC1117', 'IV6',
     & 'BD117002', 'P02MAC1117', 'SV6',
     & 'BD117003', 'P16MAC0117', 'SV616',
     & 'BD117004', 'P32MAC0117', 'SV632',
     & 'BD117005', 'P16MAC0117', 'SV816',
     & 'BD117006', 'P16MAC0117', 'IV6',
     & 'BD117007', 'P16MAC0117', 'IV8',
     & 'BD119001', 'P02MAC1119', 'IV6',
     & 'BD119002', 'P02MAC1119', 'SV6',
     & 'BD119003', 'P16MAC0119', 'SV616',
     & 'BD119004', 'P32MAC0119', 'SV632',
     & 'BD119005', 'P16MAC0119', 'SV816',
     & 'BD119006', 'P16MAC0119', 'IV6',
     & 'BD119007', 'P16MAC0119', 'IV8',
     & 'BD122001', '200DVEW122', 'SV6',
     & 'BD122002', '212DVEW122', 'SV8',
     & 'BD122003', '203DVEW122', 'SV6',
     & 'BD122004', '300DVEW122', 'SV6',
     & 'BD122005', '301DVEW122', 'SV6',
     & 'BD122006', '302DVEW122', 'SV6',
     & 'BD122007', '103DVEW122', 'SV6',
     & 'BD122008', '104DVEW122', 'SV6',
     & 'BD122009', '105DVEW122', 'SV6',
     & 'BD122010', 'P02MAC1122', 'IV6',
     & 'BD122011', 'P02MAC1122', 'SV6',
     & 'BD122012', 'R00MYE0122', 'SV6',
     & 'BD122013', 'R00MYE1122', 'SV6',
     & 'BD122014', 'R00MYE0122', 'IV6',
     & 'BD122015', 'R00MYE1122', 'IV6',
     & 'BD122016', '200DVEW122', 'IV6',
     & 'BD122017', '212DVEW122', 'IV6',
     & 'BD122018', 'P16MAC0122', 'SV616',
     & 'BD122019', 'P32MAC0122', 'SV632',
     & 'BD122020', 'P16MAC0122', 'SV816',
     & 'BD122021', 'P16MAC0122', 'IV6',
     & 'BD122022', 'P16MAC0122', 'IV8',
     & 'BD122023', '301DVEW122', 'IV6',
     & 'BD122024', '302DVEW122', 'IV6',
     & 'BD202001', '301DVEW202', 'SV6',
     & 'BD202002', '302DVEW202', 'SV6',
     & 'BD202003', 'R01KEM0202', 'SV6',
     & 'BD202004', '103DVEW202', 'SV6',
     & 'BD202005', 'P02KRU2202', 'SV6',
     & 'BD202006', 'P02MAC1202', 'IV6',
     & 'BD202007', 'P02MAC1202', 'SV6',
     & 'BD202008', 'P16MAC0202', 'SV616',
     & 'BD202009', 'P32MAC0202', 'SV632',
     & 'BD202010', 'P16MAC0202', 'SV816',
     & 'BD202011', 'P16MAC0202', 'IV6',
     & 'BD202012', 'P16MAC0202', 'IV8',
     & 'BD202013', 'P16KIN0202', 'SV616',
     & 'BD202014', 'P32KIN0202', 'SV632',
     & 'BD202015', 'P16KIN0202', 'SV816',
     & 'BD202016', 'P16KIN0202', 'IV6',
     & 'BD202017', 'P16KIN0202', 'IV8',
     & 'BD202018', '301DVEW202', 'IV6',
     & 'BD202019', '302DVEW202', 'IV6',
     & 'BD211001', 'P02KRU2211', 'SV6',
     & 'BD212001', '500DVEW212', 'SV6',
     & 'BD212002', '500DVEW212', 'SV6',
     & 'BD242002', 'P01BDE0242', 'IV6',
     & 'BD263001', 'P02KRU2263', 'SV6',
     & 'BD263002', 'P16CHA0263', 'SV616',
     & 'BD263003', 'P32CHA0263', 'SV632',
     & 'BD263004', 'P16CHA0263', 'SV816',
     & 'BD263005', 'P16CHA0263', 'IV6',
     & 'BD263006', 'P16CHA0263', 'IV8',
     & 'BD264001', 'P16BEL0264', 'SV616',
     & 'BD264002', 'P32BEL0264', 'SV632',
     & 'BD264003', 'P16BEL0264', 'SV816',
     & 'BD264004', 'P16BEL0264', 'IV6',
     & 'BD264005', 'P16BEL0264', 'IV8',
     & 'BD351001', '500DVEW351', 'SV6/8',
     & 'BD351002', '500DVEW351', 'IV6/8',
     & 'BD351003', 'P16CUR0351', 'SV616',
     & 'BD351004', 'P32CUR0351', 'SV632',
     & 'BD351005', 'P16CUR0351', 'SV816',
     & 'BD351006', 'P16CUR0351', 'IV6',
     & 'BD351007', 'P16CUR0351', 'IV8',
     & 'BD740001', 'R01KEM0740', 'SV8',
     & 'BD746001', '200DVEW746', 'SV6',
     & 'BD746002', '300DVEW746', 'SV6',
     & 'BD746003', '200DVEW746', 'IV6',
     & 'BD746004', '300DVEW746', 'IV6',
     & 'BD998001', 'P16MAC0998', 'SV616',
     & 'BD998002', 'P32MAC0998', 'SV632',
     & 'BD998003', 'P16MAC0998', 'SV816',
     & 'BD998004', 'P16MAC0998', 'IV6',
     & 'BD998005', 'P16MAC0998', 'IV8'/
      DATA ((CULIST(J,I), I=1,3), J=1,361) /
     & 'CU000006', 'R02ALN0***', 'CVT',
!     & 'CU000007', '102DVEW***', 'CVT',
     & 'CU000007', 'R02ALN1***', 'CVT',
     & 'CU000008', '102DVEW***', 'CV4',
     & 'CU000009', '101DVEW***', 'CVT',
     & 'CU000010', '101DVEW***', 'CV*',
     & 'CU000011', 'P02WEN0***', 'CVT',
     & 'CU000012', 'P02WEN0***', 'CV4',
     & 'CU000013', 'P02WEN0***', 'CV4',
     & 'CU000014', 'R01FAU0***', 'CVTS',
     & 'CU000015', 'R01FAU0***', 'TIP2',
     & 'CU000016', 'R01FAU0***', 'TIP3',
     & 'CU000017', 'R01FAU0***', 'TIP4',
     & 'CU000018', 'R01FAU0***', 'TIP5',
     & 'CU000019', 'R01FAU0***', 'TIP6',
     & 'CU000020', 'R01FAU0***', 'TIP7',
     & 'CU000021', 'R01FAU0***', 'TIP8',
     & 'CU000022', '900DVEE***', 'CVTS',
     & 'CU000023', 'P02HOR0***', 'CV4',
     & 'CU000024', '500DVEW***', 'CVT',
     & 'CU000025', '500DVEW***', 'CV4',
     & 'CU000026', '500DVEW***', 'CV9',
     & 'CU000028', 'P02WEN1***', 'CV5',
     & 'CU000029', 'P02WEN1***', 'CV6',
     & 'CU000030', 'P02WEN1***', 'CV7',
     & 'CU000031', 'P02WEN1***', 'CV8',
     & 'CU000032', 'P02WEN1***', 'CV5',
     & 'CU000033', 'P02WEN1***', 'CV6',
     & 'CU000034', 'P02WEN1***', 'CV7',
     & 'CU000035', 'P02WEN1***', 'CV8',
     & 'CU000036', 'N02BAR0***', 'CV4',
     & 'CU000037', 'P02BIG0***', 'CV4',
     & 'CU000038', 'P02BIG0***', 'CV6',
     & 'CU000039', 'N00GRE0***', 'CVT',
     & 'CU000040', 'R00MOI1***', 'CVT',
     & 'CU000043', '891CLKO***', 'CV*',
     & 'CU000044', '891CLKE***', 'CV*',
     & 'CU000045', '8**DVEE***', 'CV7/9',
     & 'CU000046', '8**DVEE***', 'CV4',
     & 'CU000047', '911DVEE***', 'CV4',
     & 'CU000048', '912DVEE***', 'CV7/9',
     & 'CU000049', '500DVEW***', 'CV*',
     & 'CU000050', 'H00SN2W***', 'CVT',
     & 'CU000051', 'H00SN2W***', 'CV4',
     & 'CU000052', 'N02SCT0***', 'CV4',
     & 'CU000053', 'N02SCT0***', 'CV7/9',
     & 'CU000054', 'N01STN0***', 'CV*',
     & 'CU000055', 'N01HAH0***', 'CV4',
     & 'CU000056', 'N01HAH0***', 'CV7/9',
     & 'CU000057', 'R00MOI0***', 'CVT',
     & 'CU000058', 'R00MOI0***', 'CV4',
     & 'CU000059', 'R00RMR0***', 'CV4',
     & 'CU000060', 'R00RMR0***', 'CVT',
     & 'CU000061', 'R00CLE0000', 'CVT',
     & 'CU000062', 'R00CLE0001', 'CVT',
     & 'CU000063', 'R01CLE0000', 'CVT',
     & 'CU000064', 'R01CLE0001', 'CVT',
     & 'CU000065', 'R01KEM0***', 'CV4',
     & 'CU000066', 'R01KEM0***', 'CVT',
     & 'CU000067', 'S00SRS0***', 'CV4',
     & 'CU000068', 'S00SRS0***', 'CVT',
     & 'CU000069', 'S00SRS0***', 'CV7/9',
     & 'CU000070', 'P16BRC2***', 'CVTS',
     & 'CU000071', 'P16BRC2***', 'CVT',
     & 'CU000072', 'P16BRC2***', 'CV4',
     & 'CU000073', 'P16BRC2***', 'CV6',
     & 'CU000074', 'P16BRC2***', 'CV8',
     & 'CU000075', 'P16BRC1***', 'CVTS',
     & 'CU000076', 'P16BRC1***', 'CVT',
     & 'CU000077', 'P16BRC1***', 'CV4',
     & 'CU000078', 'P16BRC1***', 'CV6',
     & 'CU000079', 'P16BRC1***', 'CV8',
     & 'CU000080', 'P16BRI0***', 'CVTS',
     & 'CU000081', 'P16BRI0***', 'CVT',
     & 'CU000082', 'P16BRI0***', 'CV4',
     & 'CU000083', 'P16BRI0***', 'CV6',
     & 'CU000084', 'P16BRI0***', 'CV8',
     & 'CU000085', 'P16BRO0***', 'CVTS',
     & 'CU000086', 'P16BRO0***', 'CVT',
     & 'CU000087', 'P16BRO0***', 'CV4',
     & 'CU000088', 'P16BRO0***', 'CV6',
     & 'CU000089', 'P16BRO0***', 'CV8',
     & 'CU000090', 'P16SMF0***', 'CVTS',
     & 'CU000091', 'P16SMF0***', 'CVT',
     & 'CU000092', 'P16SMF0***', 'CV4',
     & 'CU000093', 'P16SMF0***', 'CV6',
     & 'CU000094', 'P16SMF0***', 'CV8',
     & 'CU000095', 'P16PIL0***', 'CVTS',
     & 'CU000096', 'P16PIL0***', 'CVT',
     & 'CU000097', 'P16PIL0***', 'CV4',
     & 'CU000098', 'P16PIL0***', 'CV6',
     & 'CU000099', 'P16KRU1***', 'CVT',
     & 'CU000100', 'P16KRU0***', 'CVTS',
     & 'CU000101', 'P16KRU0***', 'CVT',
     & 'CU000102', 'P16KRU0***', 'CV4',
     & 'CU000103', 'P16KRU0***', 'CV6',
     & 'CU000104', 'P16KRU0***', 'CV8',
     & 'CU000105', 'A00DVEW***', 'CVT',
     & 'CU000106', 'A00DVEW***', 'CV4',
!     & 'CU000107', 'A01DVEW***', 'CVT',
     & 'CU000107', 'P01GRE0***', 'CVTS',
     & 'CU000108', 'P01GRE0***', 'CV4',
     & 'CU000109', 'A01DVEW***', 'CV4',
     & 'CU000110', 'R01KEM0***', 'CV6/8',
     & 'CU000111', 'R00RMR0***', 'CV6',
     & 'CU000112', 'R00MOI0***', 'CV6',
     & 'CU000113', 'P01GRE0***', 'CV6',
     & 'CU000114', 'P01GRE0***', 'CV8',
     & 'CU000115', 'A16DEMW***', 'CVT',
     & 'CU000116', 'P01DEM0***', 'CV*',
     & 'CU000117', 'P01EMB0***', 'CV4',
     & 'CU000118', 'P01EMB0***', 'CV6',
     & 'CU000119', 'P01DEE0***', 'CV4',
     & 'CU000120', 'P01DEE0***', 'CV6',
     & 'CU000122', 'S99BRA****', 'CV4',
     & 'CU000123', 'S99BRA****', 'CVT',
     & 'CU000124', 'P02MAC1***', 'CV4',
     & 'CU000125', 'P16MAC1***', 'CVTS',
     & 'CU000126', 'P16MAC1***', 'CVT',
     & 'CU000127', 'P16MAC1***', 'CV6',
     & 'CU000128', 'P16MAC1***', 'CV8',
     & 'CU000140', 'P03ISL****', 'CVTS',
     & 'CU000141', 'P03ISL****', 'CVTS',
     & 'CU000142', 'P03ISL****', 'CVTS',
     & 'CU000143', 'P03ISL****', 'CV4',
     & 'CU000144', 'P03ISL****', 'CV4',
     & 'CU000145', 'P03ISL****', 'CV4',
     & 'CU000146', 'P03FRU0000', 'CVTS',
     & 'CU000147', 'P03FRU0000', 'CVTS',
     & 'CU000148', 'P03FRU0000', 'CV4',
     & 'CU000149', 'P03FRU0000', 'CV4',
     & 'CU000150', 'P03CEN0000', 'CV4',
     & 'CU000199', 'P16PIL0***', 'CV8',
     & 'CU015001', '301DVEW015', 'CVT',
     & 'CU015002', '301DVEW015', 'CV*',
     & 'CU015003', '302DVEW015', 'CVT',
     & 'CU015004', '302DVEW015', 'CV*',
     & 'CU015005', 'P02MAC1015', 'CV4',
     & 'CU015006', '400MATW015', 'CVT',
     & 'CU015007', '401MATW015', 'CVT',
     & 'CU015008', 'P16MAC0015', 'CVTS',
     & 'CU015009', 'P16MAC0015', 'CVT',
     & 'CU015010', 'P16MAC0015', 'CV4',
     & 'CU015011', 'P16MAC0015', 'CV6',
     & 'CU015012', 'P16MAC0015', 'CV8',
     & 'CU019001', '400MATW019', 'CVT',
     & 'CU019002', '405MATW019', 'CVT',
     & 'CU020001', 'P02MAC1020', 'CV4',
     & 'CU020002', '400MATW020', 'CVT',
     & 'CU020003', 'P16MAC0020', 'CVTS',
     & 'CU020004', 'P16MAC0020', 'CVT',
     & 'CU020005', 'P16MAC0020', 'CV4',
     & 'CU020006', 'P16MAC0020', 'CV6',
     & 'CU020007', 'P16MAC0020', 'CV8',
     & 'CU042001', 'P01BDE0042', 'CV4',
     & 'CU042002', 'P01BDE0042', 'CV6',
     & 'CU060001', '500DVEW060', 'CV4',
     & 'CU064001', '301DVEW060', 'CV1.5',
     & 'CU064002', '302DVEW060', 'CV1.5',
     & 'CU064003', '300DVEW060', 'CV1.5',
     & 'CU064004', 'R03CHO0066', 'CV1.5',
     & 'CU064005', 'R03CHO0066', 'CV3',
     & 'CU064006', 'R03CHO0065', 'CV1.5',
     & 'CU064007', 'R03CHO0065', 'CV3',
     & 'CU064008', '400DVEW064', 'CV1.5',
     & 'CU064009', 'P16CHT0064', 'CVTS',
     & 'CU064010', 'P16CHT0064', 'CV4',
     & 'CU064011', 'P16CHT0064', 'CVT',
     & 'CU064012', 'P16CHT0064', 'CV4',
     & 'CU064013', 'P16CHT0064', 'CV6',
     & 'CU064014', 'P16CHT0064', 'CV8',
     & 'CU065001', '200DVEW065', 'CV1.5',
     & 'CU065002', '400DVEW065', 'CV1.5',
     & 'CU065003', '402DVEW065', 'CV1.5',
     & 'CU065004', '403DVEW065', 'CV1.5',
     & 'CU066001', '200DVEW066', 'CV1.5',
     & 'CU069001', '200DVEW069', 'CV1.5',
     & 'CU073001', '400MATW073', 'CVT',
     & 'CU081001', 'P02MAC1081', 'CV4',
     & 'CU081002', '400MATW081', 'CVT',
     & 'CU081003', 'P16MAC0081', 'CVTS',
     & 'CU081004', 'P16MAC0081', 'CVT',
     & 'CU081005', 'P16MAC0081', 'CV4',
     & 'CU081006', 'P16MAC0081', 'CV6',
     & 'CU081007', 'P16MAC0081', 'CV8',
     & 'CU093001', '200DVEW093', 'CVT',
     & 'CU093002', '200DVEW093', 'CV4',
     & 'CU093003', '300DVEW093', 'CVT',
     & 'CU093004', '300DVEW093', 'CV*',
     & 'CU093005', '400MATW093', 'CVT',
     & 'CU093006', '407MATW093', 'CVT',
     & 'CU093102', '200DVEW093', 'CV6',
     & 'CU094001', 'P01DIP0094', 'CV4',
     & 'CU094004', 'A00DVEW094', 'CVT',
     & 'CU094005', 'A00DVEW094', 'CV4',
     & 'CU106001', '200DVEW106', 'CV1.5',
     & 'CU106002', '301DVEW106', 'CV1.5',
     & 'CU106003', '302DVEW106', 'CV1.5',
     & 'CU106004', '300DVEW106', 'CV1.5',
     & 'CU106005', 'R03CHO0106', 'CV1.5',
     & 'CU106006', 'R03CHO0106', 'CV3',
     & 'CU108001', '200DVEW108', 'CVT',
     & 'CU108002', '200DVEW108', 'CV4',
     & 'CU108003', 'P02MAC1108', 'CV4',
     & 'CU108005', '400MATW108', 'CVT',
     & 'CU108006', '401MATW108', 'CVT',
     & 'CU108007', 'P16MAC0108', 'CVTS',
     & 'CU108008', 'P16MAC0108', 'CVT',
     & 'CU108009', 'P16MAC0108', 'CV4',
     & 'CU108010', 'P16MAC0108', 'CV6',
     & 'CU108011', 'P16MAC0108', 'CV8',
     & 'CU108102', '200DVEW108', 'CV6',
     & 'CU113001', '300DVEW113', 'CVT',
     & 'CU113002', '300DVEW113', 'CV*',
     & 'CU117001', 'P02MAC1117', 'CV4',
     & 'CU117002', '400MATW117', 'CVT',
     & 'CU117003', 'P16MAC0117', 'CVTS',
     & 'CU117004', 'P16MAC0117', 'CVT',
     & 'CU117005', 'P16MAC0117', 'CV4',
     & 'CU117006', 'P16MAC0117', 'CV6',
     & 'CU117007', 'P16MAC0117', 'CV8',
     & 'CU119003', 'P16MAC0119', 'CVTS',
     & 'CU119004', 'P16MAC0119', 'CVT',
     & 'CU119005', 'P16MAC0119', 'CV4',
     & 'CU119006', 'P16MAC0119', 'CV6',
     & 'CU119007', 'P16MAC0119', 'CV8',
     & 'CU121001', 'S00FAR0121', 'CVT',
     & 'CU121002', 'S00FAR0121', 'CV1',
     & 'CU121003', 'S00FAR0121', 'CV2',
     & 'CU121004', 'S00FAR0121', 'CV3',
     & 'CU121005', 'S00FAR0121', 'CV4',
     & 'CU121006', 'S00FAR0121', 'CV5',
     & 'CU121007', 'S00FAR0121', 'CV6',
     & 'CU121008', 'S00FAR0121', 'CV7',
     & 'CU121009', 'S00FAR0121', 'CV8',
     & 'CU121010', 'S00FAR0121', 'CV9',
     & 'CU121011', 'S00FAR0121', 'CV10',
     & 'CU121012', 'S00FAR0121', 'CV11',
     & 'CU121013', 'S00FAR0121', 'CV12',
     & 'CU122001', '200DVEW122', 'CVT',
     & 'CU122002', '200DVEW122', 'CV4',
     & 'CU122003', '212DVEW122', 'CVT',
     & 'CU122004', '212DVEW122', 'CV4',
     & 'CU122005', '203DVEW122', 'CVT',
     & 'CU122006', '300DVEW122', 'CV6',
     & 'CU122007', '301DVEW122', 'CVT',
     & 'CU122008', '301DVEW122', 'CV*',
     & 'CU122009', '302DVEW122', 'CVT',
     & 'CU122010', '302DVEW122', 'CV*',
     & 'CU122011', 'P02MAC1122', 'CV4',
     & 'CU122012', '400MATW122', 'CVT',
     & 'CU122013', '401MATW122', 'CVT',
     & 'CU122014', '402MATW122', 'CVT',
     & 'CU122015', '403MATW122', 'CVT',
     & 'CU122016', 'R00MCT0122', 'CVT',
     & 'CU122017', 'R00MCT1122', 'CVT',
     & 'CU122018', 'R00MYE0122', 'CVT',
     & 'CU122019', 'R00MYE1122', 'CVT',
     & 'CU122020', 'R00MYE0122', 'CV4',
     & 'CU122021', 'R00MYE1122', 'CV4',
     & 'CU122022', 'R00MYE0122', 'CV6',
     & 'CU122023', 'R00MYE1122', 'CV6',
     & 'CU122024', 'P16MAC0122', 'CVTS',
     & 'CU122025', 'P16MAC0122', 'CVT',
     & 'CU122026', 'P16MAC0122', 'CV4',
     & 'CU122027', 'P16MAC0122', 'CV6',
     & 'CU122028', 'P16MAC0122', 'CV8',
     & 'CU122102', '200DVEW122', 'CV6',
     & 'CU122104', '212DVEW122', 'CV6',
     & 'CU131001', 'S01BUR1131', 'CVT',
     & 'CU131002', 'S01BUR0131', 'CVT',
     & 'CU131003', 'S00BUR1131', 'CVT',
     & 'CU131004', 'S00BUR0131', 'CVT',
     & 'CU133001', '400DVEW133', 'CV1.5',
     & 'CU202001', '301DVEW202', 'CVT',
     & 'CU202002', '301DVEW202', 'CV*',
     & 'CU202003', '302DVEW202', 'CVT',
     & 'CU202004', '302DVEW202', 'CV*',
     & 'CU202005', 'R01KEM0202', 'CV4',
     & 'CU202006', 'P02KRU2202', 'CVT',
     & 'CU202007', 'P02MAC1202', 'CV4',
     & 'CU202008', 'A01BRUW202', 'CVTS',
     & 'CU202009', 'A01BRUW202', 'CVTS',
     & 'CU202010', '400MATW202', 'CVT',
     & 'CU202011', '401MATW202', 'CVT',
     & 'CU202012', '405MATW202', 'CVT',
     & 'CU202013', 'P16MAC0202', 'CVTS',
     & 'CU202014', 'P16MAC0202', 'CVT',
     & 'CU202015', 'P16MAC0202', 'CV4',
     & 'CU202016', 'P16MAC0202', 'CV6',
     & 'CU202017', 'P16MAC0202', 'CV8',
     & 'CU202018', 'P16KIN0202', 'CVTS',
     & 'CU202019', 'P16KIN0202', 'CVT',
     & 'CU202020', 'P16KIN0202', 'CV4',
     & 'CU202021', 'P16KIN0202', 'CV6',
     & 'CU202022', 'P16KIN0202', 'CV8',
     & 'CU211001', 'P02KRU2211', 'CVT',
     & 'CU212001', '500DVEW212', 'CV4',
     & 'CU212002', '500DVEW212', 'CV4',
     & 'CU242001', 'P01BDE0242', 'CV4',
     & 'CU242002', 'P01BDE0242', 'CV6',
     & 'CU263001', 'P02KRU2263', 'CVT',
     & 'CU263002', 'P16CHA0263', 'CVTS',
     & 'CU263003', 'P16CHA0263', 'CVT',
     & 'CU263004', 'P16CHA0263', 'CV4',
     & 'CU263005', 'P16CHA0263', 'CV6',
     & 'CU263006', 'P16CHA0263', 'CV8',
     & 'CU264001', 'P16BEL0264', 'CVTS',
     & 'CU264002', 'P16BEL0264', 'CVT',
     & 'CU264003', 'P16BEL0264', 'CV4',
     & 'CU264004', 'P16BEL0264', 'CV6',
     & 'CU264005', 'P16BEL0264', 'CV8',
     & 'CU300001', '200DVEW998', 'CV1.5',
     & 'CU351001', '500DVEW351', 'CV*',
     & 'CU351002', 'P16CUR0351', 'CVTS',
     & 'CU351003', 'P16CUR0351', 'CVT',
     & 'CU351004', 'P16CUR0351', 'CV4',
     & 'CU351005', 'P16CUR0351', 'CV6',
     & 'CU351006', 'P16CUR0351', 'CV8',
     & 'CU475001', '200DVEW475', 'CV1.5',
     & 'CU475002', '300DVEW314', 'CV1.5',
     & 'CU740001', 'R01KEM0740', 'CV4',
     & 'CU741001', 'P01DIP0741', 'CV4',
     & 'CU746001', '200DVEW746', 'CVT',
     & 'CU746002', '200DVEW746', 'CV4',
     & 'CU746003', '300DVEW746', 'CVT',
     & 'CU746004', '300DVEW746', 'CV*',
     & 'CU746006', '201DVEW746', 'CV4',
     & 'CU746007', 'N00SCH0746', 'CVT',
     & 'CU746008', 'N00SCH1746', 'CVT',
     & 'CU746009', 'N00SCH1746', 'TOTTREE',
     & 'CU746010', '400MATW746', 'CVT',
     & 'CU746102', '200DVEW746', 'CV6',
     & 'CU755001', '300DVEW999', 'CV1.5',
     & 'CU800001', '301DVEW800', 'CV1.5',
     & 'CU800002', '300DVEW800', 'CV1.5',
     & 'CU800003', 'N00HIL0800', 'CVT',
     & 'CU806001', 'S00CLK1806', 'CVT',
     & 'CU806002', 'S00CLK0806', 'CVT',
     & 'CU806003', 'S00CLK1806', 'CV8',
     & 'CU806004', 'S00CLK0806', 'CV8',
     & 'CU806005', 'S00CLK1806', 'CV4',
     & 'CU806006', 'S00CLK0806', 'CV4',
     & 'CU806007', 'S00CLK1806', 'CROWN',
     & 'CU806008', 'S00CLK0806', 'CROWN',
     & 'CU814001', '200DVEW814', 'CV1.5',
     & 'CU823001', '200DVEW823', 'CV1.5',
     & 'CU831001', 'S00SCH0831', 'CVT',
     & 'CU831002', 'S00SCH1831', 'CVT',
     & 'CU831003', 'S00SCH1831', 'TOTTREE',
     & 'CU833001', 'S00CLK1833', 'CVT',
     & 'CU833002', 'S00CLK0833', 'CVT',
     & 'CU833003', 'S00CLK1833', 'CV8',
     & 'CU833004', 'S00CLK0833', 'CV8',
     & 'CU833005', 'S00CLK1833', 'CV4',
     & 'CU833006', 'S00CLK0833', 'CV4',
     & 'CU833007', 'S00CLK1833', 'CROWN',
     & 'CU833008', 'S00CLK0833', 'CROWN',
     & 'CU998001', 'P16MAC0998', 'CVTS',
     & 'CU998002', 'P16MAC0998', 'CVT',
     & 'CU998003', 'P16MAC0998', 'CV4',
     & 'CU998004', 'P16MAC0998', 'CV6',
     & 'CU998005', 'P16MAC0998', 'CV8'/
     
      ERRFLG = 0
      DONE = 0
      IF(FIAEQ(1:2).EQ.'CU'.OR.FIAEQ(1:2).EQ.'cu')THEN
        IF(FIAEQ(1:2).EQ.'cu') FIAEQ(1:2) = 'CU'
          FIRST = 1
          LAST = 361
          DO 10 I = FIRST, LAST
            IF(CULIST(I,1)(1:8).EQ.FIAEQ(1:8))THEN
              NVELEQ = CULIST(I,2)(1:10)
              VOLTYPE = CULIST(I,3)
              DONE = I
              EXIT
            ENDIF
   10     CONTINUE 
      ELSEIF(FIAEQ(1:2).EQ.'BD'.OR.FIAEQ(1:2).EQ.'bd')THEN
        IF(FIAEQ(1:2).EQ.'bd') FIAEQ(1:2) = 'BD'
          FIRST = 1
          LAST = 231
          DO 20 I = FIRST, LAST
            IF(BDLIST(I,1)(1:8).EQ.FIAEQ(1:8))THEN
              NVELEQ = BDLIST(I,2)(1:10)
              VOLTYPE = BDLIST(I,3)
              DONE = I
              EXIT
            ENDIF
   20     CONTINUE 
      ELSE
        ERRFLG = 1
        RETURN
      ENDIF  
      IF(DONE.LE.0)THEN
        ERRFLG = 1
        RETURN
      ENDIF  
      IF(DONE.GT.0)THEN
!        IF(MTOPP.EQ.99.0.AND.VOLTYPE(3:3).EQ.'*')THEN
!       FOR CV*, SV* AND IV*   
        IF(MTOPP.GE.99.0)THEN     
          RETURN
        ENDIF
        IF(SPN.EQ.7503.AND.(FIAEQ(1:8).EQ.'CU000050'.OR.
     &    FIAEQ(1:8).EQ.'CU000051'.OR.FIAEQ(1:8).EQ.'BD000040'))THEN
          NVELEQ = 'H01SN2W510'
        ELSEIF((FIAEQ(1:8).EQ.'CU000116'.OR.FIAEQ(1:8).EQ.'BD000090'.OR.
     &    FIAEQ(1:8).EQ.'BD000091').AND.(SPN.EQ.98.OR.SPN.EQ.263).AND.
     &    (GEOSUB(1:1).EQ.'A'.OR.GEOSUB(1:1).EQ.'a'))THEN
          NVELEQ(7:7) = 'A'
        ELSEIF((FIAEQ(1:8).EQ.'CU000119'.OR.FIAEQ(1:8).EQ.'CU000120'.OR.
     &          FIAEQ(1:8).EQ.'CU042001'.OR.FIAEQ(1:8).EQ.'CU042002'.OR.
     &          FIAEQ(1:8).EQ.'BD042002'.OR.FIAEQ(1:8).EQ.'CU242002'.OR.
     &          FIAEQ(1:8).EQ.'CU242002'.OR.FIAEQ(1:8).EQ.'BD242002'.OR.
     &          FIAEQ(1:8).EQ.'BD000095')
     &    .AND.(GEOSUB(1:1).EQ.'A'.OR.GEOSUB(1:1).EQ.'a'))THEN
          NVELEQ(7:7) = 'A'
        ENDIF
        WRITE (SPC,'(I4)') SPN
        SPC = TRIM(ADJUSTL(SPC))
        IF(NVELEQ(7:10).EQ.'****')THEN
          IF(SPN.LT.10)THEN
            SPC = '000'//SPC
          ELSEIF(SPN.LT.100)THEN
            SPC = '00'//SPC
          ELSEIF(SPN.LT.1000)THEN
            SPC = '0'//SPC
          ENDIF
          NVELEQ = NVELEQ(1:6)//SPC
        ELSEIF(NVELEQ(8:10).EQ.'***')THEN
          IF(SPN.LT.10)THEN
            SPC = '00'//SPC
          ELSEIF(SPN.LT.100)THEN
            SPC = '0'//SPC
          ELSEIF(SPN.GT.999)THEN
            ERRFLG = 6
            RETURN
          ENDIF
          NVELEQ = NVELEQ(1:7)//SPC
        ENDIF
        GEOSUB = TRIM(ADJUSTL(GEOSUB))
        IF(NVELEQ(1:3).EQ.'8**')THEN
!       ** CAN BE 01 TO 32, DEFAULT TO 25        
          NVELEQ(1:3) = '825'
          IF(LEN(GEOSUB).EQ.1)THEN
            IF(GEOSUB(1:1).GE.'1'.AND.GEOSUB(1:1).LE.'9')THEN
              NVELEQ(1:3) = '80'//GEOSUB(1:1)
            ENDIF
          ELSEIF(LEN(GEOSUB).EQ.2)THEN
            IF((GEOSUB(1:1).GE.'0'.AND.GEOSUB(1:1).LE.'2'.AND.
     &        (GEOSUB(2:2).GE.'0'.AND.GEOSUB(2:2).LE.'9'))
     &        .OR.(GEOSUB(1:1).EQ.'3'.AND.
     &        (GEOSUB(2:2).GE.'0'.AND.GEOSUB(2:2).LE.'2')))THEN
              IF(GEOSUB(1:2).NE.'00')THEN
                NVELEQ(2:3) = GEOSUB(1:2)
              ENDIF
            ENDIF
          ENDIF
        ELSEIF(NVELEQ(1:3).EQ.'9**')THEN
          NVELEQ(1:3) = '900'
!       ** CAN BE 01,02,03,04,05,06,11,12
          IF(LEN(GEOSUB).EQ.1)THEN
            IF(GEOSUB(1:1).GE.'1'.AND.GEOSUB(1:1).LE.'6')THEN
              NVELEQ(1:3) = '90'//GEOSUB(1:1)
            ENDIF
          ELSEIF(LEN(GEOSUB).EQ.2)THEN
            IF(GEOSUB(1:1).GE.'0'.AND.
     &        (GEOSUB(2:2).GE.'1'.AND.GEOSUB(2:2).LE.'6'))THEN
              NVELEQ(2:3) = GEOSUB(1:2)
            ELSEIF(GEOSUB(1:2).EQ.'11'.OR.GEOSUB(1:2).EQ.'12')THEN
              NVELEQ(2:3) = GEOSUB(1:2)
            ENDIF
          ENDIF
        ENDIF
        IF(NVELEQ(1:2).EQ.'8*')THEN
          NVELEQ(1:2) = '89'
!         THE * CAN BE 1 TO 7
          IF(LEN(GEOSUB).EQ.1)THEN
            IF(GEOSUB(1:1).GE.'1'.AND.GEOSUB(1:1).LE.'7')THEN
              NVELEQ(1:2) = '8'//GEOSUB(1:1)
            ENDIF
          ELSEIF(LEN(GEOSUB).EQ.2.AND.GEOSUB(1:1).GE.'0')THEN
            IF(GEOSUB(2:2).GE.'1'.AND.GEOSUB(2:2).LE.'7')THEN
              NVELEQ(1:2) = '8'//GEOSUB(2:2)
            ENDIF
          ENDIF
        ENDIF
!     SET MTOPP BASED ON VOLTYPE
        IF(VOLTYPE(1:3).EQ.'CV*')THEN
          IF(MTOPP.NE.99.0)THEN
              IF(MTOPP.EQ.0.1)THEN
                VOLTYPE = 'CVT'
              ELSEIF(MTOPP.LE.4.0)THEN
                MTOPP = 4.0
                VOLTYPE = 'CV4'
              ELSE
                WRITE (TOPDC,'(I2)') NINT(MTOPP)
                IF(TOPDC(1:1).GE.'1'.AND.TOPDC(1:1).LE.'9')THEN
                  VOLTYPE = 'CV'//TOPDC
                ELSE
                  VOLTYPE = 'CV'//TOPDC(2:2)
                ENDIF
              ENDIF
          ENDIF
        ELSEIF(VOLTYPE(3:5).EQ.'7/9')THEN ! FOR CV7/9 AND SV7/9, IV7/9
          IF(SPN.LT.300)THEN
            MTOPP = 7.0
            VOLTYPE = VOLTYPE(1:2)//'7'
          ELSE
            MTOPP = 9.0
            VOLTYPE = VOLTYPE(1:2)//'9'
          ENDIF
        ELSEIF(VOLTYPE(3:5).EQ.'6/8')THEN  !FOR CV6/8, SV6/8 AND IV6/8
          IF(MTOPP.LT.6.0) MTOPP = 6.0
          IF(MTOPP.GT.9.0) MTOPP = 8.0
          WRITE (TOPDC,'(I1)') NINT(MTOPP)
          VOLTYPE = VOLTYPE(1:2)//TOPDC
        ELSEIF(VOLTYPE(1:5).EQ.'CV1.5')THEN
          MTOPP = 1.5
        ELSEIF(VOLTYPE(1:3).EQ.'SV*'.OR.VOLTYPE.EQ.'IV*')THEN
          IF(MTOPP.NE.99.0)THEN
              IF(MTOPP.LT.6.0) MTOPP = 6.0
              WRITE (TOPDC,'(I2)') NINT(MTOPP)
              IF(TOPDC(1:1).GE.'1'.AND.TOPDC(1:1).LE.'9')THEN
                  VOLTYPE = VOLTYPE(1:2)//TOPDC
              ELSE
                  VOLTYPE = VOLTYPE(1:2)//TOPDC(2:2)
              ENDIF
          ENDIF
        ELSEIF(VOLTYPE(1:3).EQ.'TIP')THEN
          READ(VOLTYPE(4:4), '(I1)') ITOP
          MTOPP = REAL(ITOP)
        ELSEIF(VOLTYPE(3:3).GE.'1'.AND.VOLTYPE(3:3).LE.'9')THEN
          IF(VOLTYPE(4:4).GE.'0'.AND.VOLTYPE(4:4).LE.'9')THEN
            READ(VOLTYPE(3:4), '(I2)') ITOP
          ELSE
            READ(VOLTYPE(3:3), '(I1)') ITOP
          ENDIF
          MTOPP = REAL(ITOP)
        ENDIF 
      ENDIF
      RETURN
      END SUBROUTINE FIAEQ2NVELEQ
! =====================================================================
      SUBROUTINE FIABEQ2NVELBEQ(BEQNUM,SPN,NVELBEQ,GEOSUB,ERRFLG)
      CHARACTER*12 NVELBEQ,COMP,GEOSUB
      CHARACTER*4 SPC
      INTEGER SPN,ERRFLG,I,J,FIRST,LAST,DONE,FIABEQNUM,BEQNUM
      CHARACTER*12 BEQLIST(124,2)
      DATA ((BEQLIST(J,I), I=1,2), J=1,124) /
     & '000001', 'FNC***AWB01D', 
     & '000002', 'FNC***MST01D', 
     & '000003', 'FNC***AWB01G', 
     & '000004', 'FNC***MST01G', 
     & '000005', 'FSE***STW01G', 
     & '000006', 'FSE***MSW01G', 
     & '000007', 'FSE***STW01D', 
     & '000008', 'FSE***MSW01D', 
     & '000009', 'FNE***STT01D', 
     & '000010', 'FNE***AWB01D', 
     & '000011', 'FRM***WB101D', 
     & '000013', 'FRM***MST01D', 
     & '000012', 'FRM***WB102D', 
     & '000014', 'FRM***MST02D', 
     & '000015', 'FRM***WB101D', 
     & '000016', 'FRM***WB201D', 
     & '000017', 'FRM***WB102D', 
     & '000018', 'FRM***WB202D', 
     & '000019', 'FNW***MSW01D', 
     & '000019', 'FNW***STW01D', 
     & '000020', 'GHZ***BRD01D', 
     & '000021', 'GHZ***BRL01D', 
     & '000022', 'GHZ***FOT01D', 
     & '000023', 'GHZ202RTT01D', 
     & '000024', 'GHZ***STB01D', 
     & '000025', 'GHZ***STW01D', 
     & '000026', 'FNW***AGTG2D', 
     & '000027', 'FNW***AWBG2D', 
     & '000028', 'FNW***AWBG1D', 
     & '000029', 'STA***FOT01D', 
     & '000030', 'STA***BRL01D', 
     & '000031', 'STA***B0Q01D', 
     & '000032', 'STA***AST01D', 
     & '000033', 'STA***STW01D', 
     & '000034', 'STA***STB01D', 
     & '000035', 'FNW***AWBSTD', 
     & '000036', 'STA204FOT01D', 
     & '000037', 'STA204BRL01D', 
     & '000038', 'STA204B0Q01D', 
     & '000039', 'STA204AST01D', 
     & '000040', 'STA204STW01D', 
     & '000041', 'STA204STB01D', 
     & '000042', 'FNW204AWBSTD', 
     & '000043', 'SHA***AGT01D', 
     & '000044', 'SHA***BRL01D', 
     & '000045', 'SHA***FOT01D', 
     & '000046', 'SHA202RTT01D', 
     & '000047', 'SHA***STB01D', 
     & '000048', 'SHA***STT01D', 
     & '000049', 'SHA***STW01D', 
     & '000050', 'FNW***AWBSHD', 
     & '000051', 'FNW***AWB51D', 
     & '000052', 'COC122BRL02D', 
     & '000053', 'COC122BRL01D', 
     & '000054', 'COC122FOT01D', 
     & '000055', 'COC122STB01D', 
     & '000056', 'COC122AGT01D', 
     & '000057', 'HAN***STB01D', 
     & '000058', 'SAC263STW01D', 
     & '000059', 'SAC263FOT01D', 
     & '000060', 'SAC263BRL01D', 
     & '000061', 'SAC263BRL02D', 
     & '000062', 'SAC263BRD01D', 
     & '000063', 'SAC263BRD02D', 
     & '000064', 'SAC263BRS01D', 
     & '000065', 'SAC263STB01D', 
     & '000066', 'SNE***BRD01D', 
     & '000067', 'SNE***CRW01D', 
     & '000068', 'SNE***STT01D', 
     & '000069', 'SNE***BRL01D', 
     & '000070', 'SNE***FOT01D', 
     & '000071', 'FNW***MSBPID', 
     & '000072', 'FNW***AWB72D', 
     & '000073', 'FRM***WB202D', 
     & '000074', 'ALM***STW01D', 
     & '000075', 'ALM***STB01D', 
     & '000076', 'ALM***FTG01D', 
     & '000077', 'ALM***BRL01D', 
     & '000078', 'ALM***BRD01D', 
     & '000079', 'ALM***AGT01D', 
     & '000080', 'ALM***STW01G', 
     & '000081', 'ALM***STB01G', 
     & '000082', 'ALM***FTG01G', 
     & '000083', 'ALM***BRL01G', 
     & '000084', 'ALM***AGT01G', 
     & '000085', 'MAN***AGT01D', 
     & '000086', 'MAN***BRL01D', 
     & '000087', 'MAN***FTG01D', 
     & '000088', 'MAN***STB01D', 
     & '000089', 'MAN***STW01D', 
     & '000090', 'MAN***AGT01G', 
     & '000091', 'MAN***BRL01G', 
     & '000092', 'MAN***FTG01G', 
     & '000093', 'MAN***STB01G', 
     & '000094', 'MAN***STW01G', 
     & '000095', 'MAN***BRD01G', 
     & '000096', 'SIN***AGT03D', 
     & '000097', 'SIN***STW03D', 
     & '000098', 'SIN***STB03D', 
     & '000099', 'SIN***B1P03D', 
     & '000100', 'SIN***FTG03D', 
     & '000101', 'SIN***AWB03D', 
     & '000102', 'FNW211STBHAD', 
     & '000103', 'WEA999AGT01D', 
     & '000104', 'SCT999AGT01D', 
     & '000105', 'BR****AGT01D', 
     & '000106', 'FRA999AGT01D', 
     & '000107', 'CIN***AGT01D', 
     & '000108', 'FRO986AGT01D', 
     & '000109', 'XXXXXXXXXX', 
     & '000110', 'SNE***STB01D', 
     & '000111', 'FNW***STB02D', 
     & '000120', 'FPI999MST01D', 
     & '000121', 'FPI999AGT*1D', 
     & '000122', 'FPI999MSW*1D', 
     & '000123', 'FPI999AGT04D', 
     & '000124', 'CAB999AGT*1D', 
     & '000125', 'CHV999AGT01D', 
     & '000126', 'ASN999AGT01D', 
     & '000127', 'AS6006AGT01D', 
     & '000128', 'AS7783AGT01D', 
     & '000129', 'AS8355AGT01D', 
     & '000130', 'FPI999MSW04D', 
     & '351001', 'MEA351STB01D'/
      
      ERRFLG = 0
      DONE = 0
      FIRST = 1
      LAST = 124
      !BEQNUM = 0
!      IF(VERIFY(TRIM(FIABEQ),'0123456789').EQ.0)THEN
!        READ (FIABEQ,'(I6)') BEQNUM
!        IF(BEQNUM.EQ.19)THEN
!          IF(GEOSUB(1:1).EQ.'4')THEN
!            NVELBEQ = 'FNW***MSW01D'
!          ELSE
!            NVELBEQ = 'FNW***STW01D'
!          ENDIF
!!          DONE = 19
!!          LAST = 1
!        ENDIF
!      ENDIF
!      IF(BEQNUM.EQ.0)THEN
!        ERRFLG = 15
!        RETURN
!      ENDIF
      !test write message
      !open(1, file = 'testdata1.dat', status = 'new')
      !write(1,*) FIABEQ
      !test start
      !NVELBEQ = 'FNW122MSW01D'
      !test end
      DO 30 I = FIRST, LAST
        READ (BEQLIST(I,1)(1:6), '(I6)') FIABEQNUM
        IF(BEQNUM.EQ.FIABEQNUM)THEN
          IF(BEQNUM.EQ.19)THEN
            IF(GEOSUB(1:1).EQ.'4')THEN
              NVELBEQ = 'FNW***MSW01D'
            ELSE
              NVELBEQ = 'FNW***STW01D'
            ENDIF
          ELSE
            NVELBEQ = BEQLIST(I,2)
          ENDIF
          DONE = I
          EXIT
        ENDIF
   30 CONTINUE 
      IF(DONE.GT.0)THEN
        WRITE (SPC,'(I4)') SPN
        SPC = TRIM(ADJUSTL(SPC))
        IF(NVELBEQ(3:6).EQ.'****')THEN
          IF(SPN.LT.10)THEN
            SPC = '000'//SPC
          ELSEIF(SPN.LT.100)THEN
            SPC = '00'//SPC
          ELSEIF(SPN.LT.1000)THEN
            SPC = '0'//SPC
          ENDIF
          NVELBEQ(3:6) = SPC
        ELSEIF(NVELBEQ(4:6).EQ.'***')THEN
          IF(SPN.LT.10)THEN
            SPC = '00'//SPC
          ELSEIF(SPN.LT.100)THEN
            SPC = '0'//SPC
          ELSEIF(SPN.GT.999)THEN
            ERRFLG = 6
            RETURN
          ENDIF
          NVELBEQ(4:6) = SPC
        ENDIF
        IF(NVELBEQ(10:10).EQ.'*')THEN
          IF(GEOSUB(1:1).GT.'0'.AND.GEOSUB(1:1).LE.'3')THEN
            NVELBEQ(10:10) = GEOSUB(1:1)
          ELSE
            NVELBEQ(10:10) = '0'
          ENDIF
        ENDIF
        !FIABEQ(1:12) = NVELBEQ(1:12)
      ELSE
        ERRFLG = 15
      ENDIF
      RETURN
      END    