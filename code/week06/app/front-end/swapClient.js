import Wallet from './wallet'

// elements
let divSub = document.querySelector('div.sub');
let sectionSub = document.createElement('section');
let input = document.createElement('input');
let addWalletBtn = document.createElement('button');
// fill elements
input.placeholder = 'Wallet #';
addWalletBtn.textContent = 'Add Wallet';
// render elements
sectionSub.setAttribute('class', 'fade-in2');
divSub.appendChild(sectionSub);
sectionSub.appendChild(input);
sectionSub.appendChild(addWalletBtn);
let wallets = [];
let walletCount = 0;
/*
// UUID check
let isUUID = false
const checkUUID = async(wallet) => { // returns true if UUID is valid
  isUUID = false;
  let url = `../../W${wallet}.cid`;
  const textFetch = async () => {
    let response = await fetch(url);
    if(!response.ok) {
      throw Error(`uuid not found`);
    }
    return await response.text();
  }
  const validateUUID = async (text) => {
    for(let i = 0; i < text.length;) { // check if uuid is valid
      if((((i !== 8 || i !== 13 || i !== 18 || i !== 23) && text[i] !== '-') ||
          ((i === 8 || i === 13 || i === 18 || i === 23) && text[i] === '-')) &&
          i < text.length-1 && text.length === 36) {
        i++;
      } else if (i === text.length-1 && text[i] !== '-'){
        // console.log('uuid found');
        isUUID = true;
        return true;
      } else {
        throw Error ('not a valid uuid');
      }
    }
  }
  textFetch()
    .then(text => validateUUID(text))
    .catch(err => console.log(err));
}
*/
// New Wallet
/*
const newWallet = async (wallet, key) => {
  wallets.push(new Wallet(wallet, key));
}
*/
const addWallet = async (walletNumber, key) => {
  let tempWallet = {};
  let newWallet = async function() {
    return new Wallet(walletNumber,key);
  }
  let fetchUUID = async function(wallet) {
   await wallet.fetchUUID();
  }
  let displayWallet = async function(wallet) {
    return await wallet.displayWallet();
  }
  let calibrateWallet = async function(wallet) {
    wallets.push(wallet);
    walletCount++;
    input.value='';
    return await wallet;
  }
  let asyncLog = async function(string) {
    console.log(await string);
  }
  let checkWallet = async function(wallet) {
    await asyncLog(await wallet.walletName + ' uuid: ' + await wallet.uuid);
    return await wallet;
  }
  if(wallets.find(elem => elem.walletNumber == walletNumber) === undefined) { // check to see if Wallet Number is used
    tempWallet = await newWallet()
      .then(async wallet => await fetchUUID(wallet))
      .catch(e => console.log(e));
    if(await tempWallet.hasUUID) {
      displayWallet(wallet);
      //.then(async wallet => await calibrateWallet(wallet))
      //.then(async wallet => await checkWallet(wallet));
    } else {
      console.log('fail ' + await tempWallet.hasUUID);
    }

  /*
    newWallet(wallet, key).then(() => {})
      .then(async () => {
        await wallets[walletCount].displayWallet();
        console.log(await wallets[walletCount].walletName);
        await wallets[walletCount].fetchUUID();
        if(await wallets[walletCount]) {
          walletCount++;
          input.value='';
        }
      })
      .then(() => {})
      //.then(async () => console.log('hasUUID: ' + await wallets[walletCount-1].hasUUID))
      .then(() => console.log('uuid: ' + wallets[walletCount -1].walletName))
      .catch(e => {
        console.log(e);
      });

    //if(await wallets[walletCount-1].hasUUID) {
      //console.log(await wallets[walletCount-1].hasUUID);
      //wallets[walletCount].displayWallet();
      //walletCount++; // sets key
      //input.value = ''; // clear input
    //} else {
      //wallets.pop();
    //}
    */
  } else {
    console.log('wallet already exists');
  }
}


addWalletBtn.addEventListener('click', () => {
   //await checkUUID(input.value)
   // .then(addWallet(input.value, walletCount))
  addWallet(input.value, walletCount)
    .catch(e => {
      console.log(e); // reports invalid uuid
      //input.value = ''; // clear input
      //wallets.pop();
    });
});

let checkBtn = document.createElement('button');
checkBtn.textContent = 'Check';
divSub.appendChild(checkBtn);
checkBtn.addEventListener('click', () => {
  // console.log(isUUID);
  wallets.forEach(elem => console.log(elem.uuid));
});

/*
class Wallet {
  constructor(walletNumber, id) {
    // elements (containers)
    this.main = document.querySelector('main');
    this.div = document.createElement('div');
    this.article = document.createElement('article');
    this.section1 = document.createElement('section');
    this.section2 = document.createElement('section');
    this.span = document.createElement('span');
    // elements
    this.walletName = document.createElement('h2');
    this.oracleName = document.createElement('h2');
    this.oracleDescription = document.createElement('h4');
    this.getFundsBtn = document.createElement('button');
    this.offerInput = document.createElement('input');
    this.offerInputBtn = document.createElement('button');
    this.retrieveBtn = document.createElement('button');
    this.useBtn = document.createElement('button');
    // fill elements
    this.walletName.textContent = `Wallet ${walletNumber}`;
    this.oracleName.textContent = 'Oracle';
    this.oracleDescription.textContent = '- USDA to ADA -';
    this.getFundsBtn.textContent = 'Read Funds';
    this.offerInput.placeholder = 'Offer Amount';
    this.offerInputBtn.textContent = 'Offer Funds';
    this.retrieveBtn.textContent = 'Retrieve Funds';
    this.useBtn.textContent = 'Use Oracle';
    // configure elements
    this.div.setAttribute('class', 'fade-in1');
    this.div.setAttribute('id', 'wallet');
    // data
    this.walletNumber = walletNumber
    this.key = id;
    this.hasUUID = undefined;
    // this.uuid = this.fetchUUID(this.walletNumber);
    this.uuid = undefined;
  }

  async fetchUUID(){
    let url = `../../W${this.walletNumber}.cid`; // compose URL
    const textFetch = async () => {
      let response = await fetch(url);
      if(!response.ok) {
        this.hasUUID = false;
        throw Error(`${response.status}: ${response.statusText}`);
      }
      return await response.text();
    }
    const validateUUID = async (text) => {
      for(let i = 0; i < text.length;) { // check if uuid is valid
        if((((i !== 8 || i !== 13 || i !== 18 || i !== 23) && text[i] !== '-') ||
            ((i === 8 || i === 13 || i === 18 || i === 23) && text[i] === '-')) &&
            i < text.length-1 && text.length === 36) {
          i++;
        } else if (i === text.length-1 && text[i] !== '-'){
          // console.log('uuid found');
          this.hasUUID = true;
          return await text;
        } else {
          this.hasUUID = false;
          throw Error ('not a valid uuid');
        }
      }
    }
    const extractText = async (text) => { // extract uuid from validaton
      console.log(`successfully created Wallet ${this.walletNumber} uuid: ${await text}`);
      this.uuid = text;
    }
    textFetch() // deploy fetch
      .then(text => validateUUID(text))
      .then(text => extractText(text))
      .catch(err => console.log(err));
  }
  displayWallet() {
    // render elements
    this.main.appendChild(this.walletName);
    this.main.appendChild(this.div);
    this.div.appendChild(this.article);
    this.article.appendChild(this.section1);
    this.article.appendChild(this.section2);
    // section1 render
    this.section1.appendChild(this.span);
    this.span.appendChild(this.oracleName);
    this.span.appendChild(this.oracleDescription);
    this.section1.appendChild(this.offerInput);
    this.section1.appendChild(this.offerInputBtn);
    // section2 render
    this.section2.appendChild(this.getFundsBtn);
    this.section2.appendChild(this.retrieveBtn);
    this.section2.appendChild(this.useBtn);
  }
}
*/
