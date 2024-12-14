Priority: Accuracy
├── If Sparsity Level = High → LARS
├── Else If Data Size = Medium AND Sparsity Level = Medium → FISTA
├── Else If Data Size = Large AND Sparsity Level = Low → ISTA
└── Default → LARS

Priority: Speed
├── If Data Size = Very Large OR Sparsity Level = High → PFA
├── Else If Data Size = Medium OR Large AND feature size = Medium → CGDA
├── Else If feature Size = low → FISTA
└── Default → ISTA

Priority: Sparsity
├── If Feature Size = low AND Sparsity Level = High → PFA
├── Else If Sparsity Level = Medium → CGDA
├── Else If Sparsity Level = Low → FISTA
└── Default → CGDA
