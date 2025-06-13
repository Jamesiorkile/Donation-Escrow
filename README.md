# Donation Escrow for NGOs

A Clarity smart contract that enables secure donations to NGO campaigns with built-in escrow functionality. Funds are only released when campaign goals are met and verified.

## Features

- Create donation campaigns with fundraising goals and deadlines
- Secure donation escrow with conditional fund release
- Verification system to ensure campaign legitimacy
- Automatic refunds if campaign goals aren't met
- Transparent tracking of campaign progress

## How It Works

1. **Campaign Creation**: NGOs can create campaigns with a name, description, funding goal, and deadline.
2. **Donations**: Donors can contribute STX to campaigns they want to support.
3. **Verification**: Trusted verifiers can mark campaigns as verified.
4. **Fund Release**: When a campaign reaches its goal and is verified, the NGO can claim the funds.
5. **Refunds**: If a campaign doesn't reach its goal by the deadline, donors can claim refunds.

## Contract Functions

### For NGOs

- `create-campaign`: Create a new fundraising campaign
- `complete-campaign`: Mark a campaign as completed when goal is reached
- `release-funds`: Withdraw funds after campaign completion

### For Donors

- `donate`: Contribute STX to a campaign
- `claim-refund`: Retrieve donations if campaign fails to meet its goal

### For Verifiers

- `verify-campaign`: Mark a campaign as legitimate

### For Contract Owner

- `add-verifier`: Add a trusted verifier
- `remove-verifier`: Remove a verifier

## Usage Examples

### Creating a Campaign

```clarity
(contract-call? .donation-escrow create-campaign "Clean Water Project" "Providing clean water to rural communities" u10000000 u50000)
```

### Making a Donation

```clarity
(contract-call? .donation-escrow donate u1 u500000)
```

### Verifying a Campaign

```clarity
(contract-call? .donation-escrow verify-campaign u1)
```

### Completing a Campaign and Releasing Funds

```clarity
(contract-call? .donation-escrow complete-campaign u1)
(contract-call? .donation-escrow release-funds u1)
```

### Claiming a Refund

```clarity
(contract-call? .donation-escrow claim-refund u1)
```

## Security Considerations

- Funds are held in escrow until campaign goals are met
- Only verified campaigns can receive funds
- Automatic refund mechanism protects donors
- Multi-step release process prevents unauthorized withdrawals

