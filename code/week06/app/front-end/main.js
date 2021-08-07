import Wallet from './modules/wallet.js'

// elements
let main = document.querySelector('main');
let divSub = document.querySelector('div.sub');
let sectionSub1 = document.createElement('section');
sectionSub1.setAttribute('class', 'add');
let sectionSub2 = document.createElement('section');
sectionSub2.setAttribute('class', 'delete');
let input = document.createElement('input');
let addWalletBtn = document.createElement('button');
let deleteList = document.createElement('select');
let deleteBtn = document.createElement('button');
// fill elements
input.placeholder = 'Wallet #';
addWalletBtn.textContent = 'Add Wallet';
deleteBtn.textContent = 'Delete Wallet';
// render elements
sectionSub1.setAttribute('class', 'fade-in2');
divSub.appendChild(sectionSub1);
sectionSub1.appendChild(input);
sectionSub1.appendChild(addWalletBtn);
let wallets = [];
let walletCount = 0;
let walletElements = [];
// New Wallet
const addWallet = async (walletNumber, key) => {
  let newWallet = async function(wallet) {
    wallets.push(await wallet);
  }
  if(wallets.find(elem => elem.walletNumber == walletNumber) === undefined) { // check to see if Wallet Number is used
    let tempWallet = await new Wallet(walletNumber, key)
      .then(async wallet => {
        if(wallet.hasUUID) {
          await newWallet(wallet);
        }})
      .catch(e => console.log(e));
  } else {
    console.log('wallet already exists');
  }
}
addWalletBtn.addEventListener('click', async () => {
  await addWallet(input.value, walletCount)
    .catch(e => {
      console.log(e); // reports invalid uuid
    });
  /* // figure out how to select a wallet to delete
  if(wallets.length > 0) {
    divSub.appendChild(sectionSub2);
    sectionSub2.appendChild(deleteList);
    sectionSub2.appendChild(deleteBtn);
  }
  let newOption = document.createElement('option');
  newOption.value = input.value;
  newOption.textContent = input.value;
  deleteList.appendChild(newOption);
  */
  input.value = '';
  walletCount++;
});

/* // figure out how to select wallet to delete
deleteBtn.addEventListener('click', () => {
  console.log(wallets.find(elem => elem.walletNumber === deleteList.value));
  let tempWallets = wallets.filter(elem => elem.walletNumber !== deleteList.value);
  console.log(tempWallets);
  wallets = tempWallets;
});
*/


/*

let checkBtn = document.createElement('button');
checkBtn.textContent = 'Check';
divSub.appendChild(checkBtn);
checkBtn.addEventListener('click', () => {
  // console.log(isUUID);
  //wallets.forEach(elem => console.log(elem.walletName));
  console.log(wallets);
});
*/
