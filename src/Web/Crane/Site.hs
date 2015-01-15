module Web.Crane.Site where

import Web.Crane.Types
import Web.Crane.Session

defaultSiteSpec :: CraneApp a => a -> CraneSiteSpec a
defaultSiteSpec a = CraneSiteSpec
                    { mkSessionBackend = mkMemorySessionBackend
                    , rootApplication  = a }

-- | given a specification, construct everything we need to run the website
mkSite :: CraneSiteSpec a -> IO (CraneSite, a)
mkSite siteSpec = do
  sessionBackend <- mkSessionBackend siteSpec
  return ( CraneSite { csSessionBackend = sessionBackend }
         , rootApplication siteSpec)