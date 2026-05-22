-- | Profile realization: turns a 'TransportConfig' into concrete
-- OS-level tuning actions.
--
-- Currently a stub.  Full implementation involves CPU pinning
-- (sched_setaffinity), huge page setup (MAP_HUGETLB), NUMA placement
-- (mbind), and mlock.
module Wireform.Network.Transport.Profile
  ( realizeProfile
  ) where

import Wireform.Transport.Config
import Wireform.Transport.Capabilities

-- | Given a profile and detected capabilities, produce a config
-- that respects what the system can actually do.
realizeProfile :: Profile -> SystemCapabilities -> TransportConfig
realizeProfile p _caps = profileConfig p
