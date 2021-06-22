# Object-Centric_Trace_Clustering
A network embedding approach for Object-Centric Trace Clustering, submitted to ICPM 2021


The trace clustering problem in Process Mining uses an event log, where there is one clear case identifier, to group cases into homogeneous groups. However, the reality brought forward by the \textit{Object-centric Process Mining} suggests that a business process may involve multiple objects in a way that no single case notion exists. This situation renders the existing trace clustering methods unfitting for object-centric business processes.

The goal of this work is to turn the existing methods of trace clustering relevant for trace clustering problems when processes are described by an object-centric event log. We propose an embedding representation that captures simultaneously the similarity of traces within the objects of the same object type, as well as the relationships between the objects of different types. We formulate an optimization problem that involves the similarity matrices, the cross-objects types relationships matrices, and the embeddings. Then, we follow an iterative algorithm to optimize it and deliver the embedding representation, and eventually the cluster memberships for each object type.
This work presents the fundamental formulation towards this goal and a proof of concept that demonstrates the applicability of the proposed approach.

This is the script file that implements the algorithm described in the paper submitted to ICPM 2021.
