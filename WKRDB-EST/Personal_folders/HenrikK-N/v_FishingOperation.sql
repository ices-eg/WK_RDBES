USE [RES]
GO

/****** Object:  View [dbo].[vFishingOperation]    Script Date: 08/10/2019 14:56:33 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


AlTEr VIEW [dbo].[vFishingOperation]
AS
/****** Script for SelectTopNRows command from SSMS  ******/
SELECT 
FOid
      ,FTid
      ,SDid
      ,FOrecordType
      ,fst.Code FOstratification
      ,FOhaulNumber
      ,FOstratum
      ,fcl.Code FOclustering
      ,FOclusterName
      ,smr.Code FOsampler
      ,agl.Description FOaggregationLevel
      ,val.Code FOvalidity
      ,ctr.Code FOcatchReg
      ,FOstartDate
      ,FOstartTime
      ,FOendDate
      ,FOendTime
      ,dur.Code FOduration
      ,FOstartLat
      ,FOstartLon
      ,FOstopLat
      ,FOstopLon
      ,ecz.Code FOeconomicalZone
      ,are.Code FOarea
      ,rec.Code FOrectangle
      ,FOsubpolygon
      ,FOfunctinalUnit
      ,FOfishingDepth
      ,FOwaterDepth
      ,nac.Code FOnationalCategory
      ,me5.Code FOmetier5
      ,me6.Code FOmetier6
      ,gea.Code FOgear
      ,FOmeshSize
      ,sdv.Code FOselectionDevice
      ,FOselectionDeviceMeshSize
      ,tsp.Code FOtargetSpecies
      ,obc.Code FOobservationCode
      ,FOtotal
      ,FOsampled
      ,FOsampProb
      ,slm.Code FOselectionMethod
	  --,FOselectionMethod
      ,smc.Code FOselectionMethodCluster
      ,FOtotalClusters
      ,FOsampledClusters
      ,FOclustersProb
      ,rns.Code FOreasonNotSampled
  FROM RES.dbo.FishingOperation
    LEFT OUTER JOIN tblCode fst ON fst.tblCodeID = FishingOperation.FOstratification
	    LEFT OUTER JOIN tblCode fcl ON fcl.tblCodeID = FishingOperation.FOclustering
		LEFT OUTER JOIN tblCode smr ON smr.tblCodeID = FishingOperation.FOsampler
  	LEFT OUTER JOIN tblCode agl ON agl.tblCodeID = FishingOperation.FOaggregationLevel
	  	LEFT OUTER JOIN tblCode val ON val.tblCodeID = FishingOperation.FOvalidity
		LEFT OUTER JOIN tblCode ctr ON ctr.tblCodeID = FishingOperation.FOcatchReg
		LEFT OUTER JOIN tblCode dur ON dur.tblCodeID = FishingOperation.FOduration
		LEFT OUTER JOIN tblCode ecz ON ecz.tblCodeID = FishingOperation.FOeconomicalZone
		LEFT OUTER JOIN tblCode are ON are.tblCodeID = FishingOperation.FOarea	
		LEFT OUTER JOIN tblCode rec ON rec.tblCodeID = FishingOperation.FOrectangle
		LEFT OUTER JOIN tblCode nac ON nac.tblCodeID = FishingOperation.FOnationalCategory
		LEFT OUTER JOIN tblCode me5 ON me5.tblCodeID = FishingOperation.FOmetier5
		LEFT OUTER JOIN tblCode me6 ON me6.tblCodeID = FishingOperation.FOmetier5
		LEFT OUTER JOIN tblCode gea ON gea.tblCodeID = FishingOperation.FOgear
		LEFT OUTER JOIN tblCode sdv ON sdv.tblCodeID = FishingOperation.FOselectionDevice
		LEFT OUTER JOIN tblCode tsp ON tsp.tblCodeID = FishingOperation.FOtargetSpecies
		LEFT OUTER JOIN tblCode obc ON obc.tblCodeID = FishingOperation.FOobservationCode
		
		LEFT OUTER JOIN tblCode slm ON slm.tblCodeID = FishingOperation.FOselectionMethod

		LEFT OUTER JOIN tblCode smc ON smc.tblCodeID = FishingOperation.FOselectionMethodCluster	
		LEFT OUTER JOIN tblCode rns ON rns.tblCodeID = FishingOperation.FOreasonNotSampled
		

GO


