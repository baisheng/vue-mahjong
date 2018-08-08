/**
 * Created by LISHI on 2017/4/26.
 */
import * as PIXI from 'pixi.js'

PIXI.utils.skipHello()

const [w, h] = [window.innerWidth, window.innerHeight]
const Ratio = window.devicePixelRatio
const W = w * Ratio
const H = h * Ratio


import store from '../../store'


const TWEEN = require('tween.js');


export default class Game {

  static getInstance() {

    if (!Game.instance) {
      Game.instance = new Game()
    }
    return Game.instance
  }

  constructor() {
    this.app = new PIXI.Application(window.innerWidth * Ratio, window.innerHeight * Ratio, {
      transparent: true,
    });
    this.isHaveLoad = false
    this.isLoadFirst = false
    this.isLoadTouzi = false
  }


  loadRes(cb) {

    if (this.isLoadFirst) {
      cb && cb()
      return false
    }

    this.isLoadFirst = true
    PIXI.loader
      .add('p1', require('@/assets/card/1.png'))
      .add('p2', require('@/assets/card/2.png'))
      .add('p3', require('@/assets/card/3.png'))
      .add('p4', require('@/assets/card/4.png'))
      .add('p5', require('@/assets/card/5.png'))
      .add('p6', require('@/assets/card/6.png'))
      .add('p7', require('@/assets/card/7.png'))
      .add('p8', require('@/assets/card/8.png'))
      .add('p11', require('@/assets/card/11.png'))
      .add('p12', require('@/assets/card/12.png'))
      .add('p13', require('@/assets/card/13.png'))
      .add('p14', require('@/assets/card/14.png'))
      .add('p15', require('@/assets/card/15.png'))
      .add('p16', require('@/assets/card/16.png'))
      .add('p17', require('@/assets/card/17.png'))
      .add('p18', require('@/assets/card/18.png'))
      .add('p19', require('@/assets/card/19.png'))
      .add('p21', require('@/assets/card/21.png'))
      .add('p22', require('@/assets/card/22.png'))
      .add('p23', require('@/assets/card/23.png'))
      .add('p24', require('@/assets/card/24.png'))
      .add('p25', require('@/assets/card/25.png'))
      .add('p26', require('@/assets/card/26.png'))
      .add('p27', require('@/assets/card/27.png'))
      .add('p28', require('@/assets/card/28.png'))
      .add('p29', require('@/assets/card/29.png'))
      .add('p31', require('@/assets/card/31.png'))
      .add('p32', require('@/assets/card/32.png'))
      .add('p33', require('@/assets/card/33.png'))
      .add('p34', require('@/assets/card/34.png'))
      .add('p35', require('@/assets/card/35.png'))
      .add('p36', require('@/assets/card/36.png'))
      .add('p37', require('@/assets/card/37.png'))
      .add('p38', require('@/assets/card/38.png'))
      .add('p39', require('@/assets/card/39.png'))
      .add('p41', require('@/assets/card/41.png'))
      .add('p42', require('@/assets/card/42.png'))
      .add('p43', require('@/assets/card/43.png'))
      .add('p44', require('@/assets/card/44.png'))
      .add('p51', require('@/assets/card/51.png'))
      .add('p52', require('@/assets/card/52.png'))
      .add('p53', require('@/assets/card/53.png'))
      .add('b1', require('@/assets/card1/1.png'))
      .add('b2', require('@/assets/card1/2.png'))
      .add('b3', require('@/assets/card1/3.png'))
      .add('b4', require('@/assets/card1/4.png'))
      .add('b5', require('@/assets/card1/5.png'))
      .add('b6', require('@/assets/card1/6.png'))
      .add('b7', require('@/assets/card1/7.png'))
      .add('b8', require('@/assets/card1/8.png'))
      .add('b11', require('@/assets/card1/11.png'))
      .add('b12', require('@/assets/card1/12.png'))
      .add('b13', require('@/assets/card1/13.png'))
      .add('b14', require('@/assets/card1/14.png'))
      .add('b15', require('@/assets/card1/15.png'))
      .add('b16', require('@/assets/card1/16.png'))
      .add('b17', require('@/assets/card1/17.png'))
      .add('b18', require('@/assets/card1/18.png'))
      .add('b19', require('@/assets/card1/19.png'))
      .add('b21', require('@/assets/card1/21.png'))
      .add('b22', require('@/assets/card1/22.png'))
      .add('b23', require('@/assets/card1/23.png'))
      .add('b24', require('@/assets/card1/24.png'))
      .add('b25', require('@/assets/card1/25.png'))
      .add('b26', require('@/assets/card1/26.png'))
      .add('b27', require('@/assets/card1/27.png'))
      .add('b28', require('@/assets/card1/28.png'))
      .add('b29', require('@/assets/card1/29.png'))
      .add('b31', require('@/assets/card1/31.png'))
      .add('b32', require('@/assets/card1/32.png'))
      .add('b33', require('@/assets/card1/33.png'))
      .add('b34', require('@/assets/card1/34.png'))
      .add('b35', require('@/assets/card1/35.png'))
      .add('b36', require('@/assets/card1/36.png'))
      .add('b37', require('@/assets/card1/37.png'))
      .add('b38', require('@/assets/card1/38.png'))
      .add('b39', require('@/assets/card1/39.png'))
      .add('b41', require('@/assets/card1/41.png'))
      .add('b42', require('@/assets/card1/42.png'))
      .add('b43', require('@/assets/card1/43.png'))
      .add('b44', require('@/assets/card1/44.png'))
      .add('b51', require('@/assets/card1/51.png'))
      .add('b52', require('@/assets/card1/52.png'))
      .add('b53', require('@/assets/card1/53.png'))
      .add('j1', require('@/assets/card2/1.png'))
      .add('j2', require('@/assets/card2/2.png'))
      .add('j3', require('@/assets/card2/3.png'))
      .add('j4', require('@/assets/card2/4.png'))
      .add('j5', require('@/assets/card2/5.png'))
      .add('j6', require('@/assets/card2/6.png'))
      .add('j7', require('@/assets/card2/7.png'))
      .add('j8', require('@/assets/card2/8.png'))
      .add('j11', require('@/assets/card2/11.png'))
      .add('j12', require('@/assets/card2/12.png'))
      .add('j13', require('@/assets/card2/13.png'))
      .add('j14', require('@/assets/card2/14.png'))
      .add('j15', require('@/assets/card2/15.png'))
      .add('j16', require('@/assets/card2/16.png'))
      .add('j17', require('@/assets/card2/17.png'))
      .add('j18', require('@/assets/card2/18.png'))
      .add('j19', require('@/assets/card2/19.png'))
      .add('j21', require('@/assets/card2/21.png'))
      .add('j22', require('@/assets/card2/22.png'))
      .add('j23', require('@/assets/card2/23.png'))
      .add('j24', require('@/assets/card2/24.png'))
      .add('j25', require('@/assets/card2/25.png'))
      .add('j26', require('@/assets/card2/26.png'))
      .add('j27', require('@/assets/card2/27.png'))
      .add('j28', require('@/assets/card2/28.png'))
      .add('j29', require('@/assets/card2/29.png'))
      .add('j31', require('@/assets/card2/31.png'))
      .add('j32', require('@/assets/card2/32.png'))
      .add('j33', require('@/assets/card2/33.png'))
      .add('j34', require('@/assets/card2/34.png'))
      .add('j35', require('@/assets/card2/35.png'))
      .add('j36', require('@/assets/card2/36.png'))
      .add('j37', require('@/assets/card2/37.png'))
      .add('j38', require('@/assets/card2/38.png'))
      .add('j39', require('@/assets/card2/39.png'))
      .add('j41', require('@/assets/card2/41.png'))
      .add('j42', require('@/assets/card2/42.png'))
      .add('j43', require('@/assets/card2/43.png'))
      .add('j44', require('@/assets/card2/44.png'))
      .add('j51', require('@/assets/card2/51.png'))
      .add('j52', require('@/assets/card2/52.png'))
      .add('j53', require('@/assets/card2/53.png'))
      .add('s1', require('@/assets/card3/1.png'))
      .add('s2', require('@/assets/card3/2.png'))
      .add('s3', require('@/assets/card3/3.png'))
      .add('s4', require('@/assets/card3/4.png'))
      .add('s5', require('@/assets/card3/5.png'))
      .add('s6', require('@/assets/card3/6.png'))
      .add('s7', require('@/assets/card3/7.png'))
      .add('s8', require('@/assets/card3/8.png'))
      .add('s11', require('@/assets/card3/11.png'))
      .add('s12', require('@/assets/card3/12.png'))
      .add('s13', require('@/assets/card3/13.png'))
      .add('s14', require('@/assets/card3/14.png'))
      .add('s15', require('@/assets/card3/15.png'))
      .add('s16', require('@/assets/card3/16.png'))
      .add('s17', require('@/assets/card3/17.png'))
      .add('s18', require('@/assets/card3/18.png'))
      .add('s19', require('@/assets/card3/19.png'))
      .add('s21', require('@/assets/card3/21.png'))
      .add('s22', require('@/assets/card3/22.png'))
      .add('s23', require('@/assets/card3/23.png'))
      .add('s24', require('@/assets/card3/24.png'))
      .add('s25', require('@/assets/card3/25.png'))
      .add('s26', require('@/assets/card3/26.png'))
      .add('s27', require('@/assets/card3/27.png'))
      .add('s28', require('@/assets/card3/28.png'))
      .add('s29', require('@/assets/card3/29.png'))
      .add('s31', require('@/assets/card3/31.png'))
      .add('s32', require('@/assets/card3/32.png'))
      .add('s33', require('@/assets/card3/33.png'))
      .add('s34', require('@/assets/card3/34.png'))
      .add('s35', require('@/assets/card3/35.png'))
      .add('s36', require('@/assets/card3/36.png'))
      .add('s37', require('@/assets/card3/37.png'))
      .add('s38', require('@/assets/card3/38.png'))
      .add('s39', require('@/assets/card3/39.png'))
      .add('s41', require('@/assets/card3/41.png'))
      .add('s42', require('@/assets/card3/42.png'))
      .add('s43', require('@/assets/card3/43.png'))
      .add('s44', require('@/assets/card3/44.png'))
      .add('s51', require('@/assets/card3/51.png'))
      .add('s52', require('@/assets/card3/52.png'))
      .add('s53', require('@/assets/card3/53.png'))
      .add('t1', require('@/assets/card4/1.png'))
      .add('t2', require('@/assets/card4/2.png'))
      .add('t3', require('@/assets/card4/3.png'))
      .add('t4', require('@/assets/card4/4.png'))
      .add('t5', require('@/assets/card4/5.png'))
      .add('t6', require('@/assets/card4/6.png'))
      .add('t7', require('@/assets/card4/7.png'))
      .add('t8', require('@/assets/card4/8.png'))
      .add('t11', require('@/assets/card4/11.png'))
      .add('t12', require('@/assets/card4/12.png'))
      .add('t13', require('@/assets/card4/13.png'))
      .add('t14', require('@/assets/card4/14.png'))
      .add('t15', require('@/assets/card4/15.png'))
      .add('t16', require('@/assets/card4/16.png'))
      .add('t17', require('@/assets/card4/17.png'))
      .add('t18', require('@/assets/card4/18.png'))
      .add('t19', require('@/assets/card4/19.png'))
      .add('t21', require('@/assets/card4/21.png'))
      .add('t22', require('@/assets/card4/22.png'))
      .add('t23', require('@/assets/card4/23.png'))
      .add('t24', require('@/assets/card4/24.png'))
      .add('t25', require('@/assets/card4/25.png'))
      .add('t26', require('@/assets/card4/26.png'))
      .add('t27', require('@/assets/card4/27.png'))
      .add('t28', require('@/assets/card4/28.png'))
      .add('t29', require('@/assets/card4/29.png'))
      .add('t31', require('@/assets/card4/31.png'))
      .add('t32', require('@/assets/card4/32.png'))
      .add('t33', require('@/assets/card4/33.png'))
      .add('t34', require('@/assets/card4/34.png'))
      .add('t35', require('@/assets/card4/35.png'))
      .add('t36', require('@/assets/card4/36.png'))
      .add('t37', require('@/assets/card4/37.png'))
      .add('t38', require('@/assets/card4/38.png'))
      .add('t39', require('@/assets/card4/39.png'))
      .add('t41', require('@/assets/card4/41.png'))
      .add('t42', require('@/assets/card4/42.png'))
      .add('t43', require('@/assets/card4/43.png'))
      .add('t44', require('@/assets/card4/44.png'))
      .add('t51', require('@/assets/card4/51.png'))
      .add('t52', require('@/assets/card4/52.png'))
      .add('t53', require('@/assets/card4/53.png'))
      .add('np1', require('@/assets/card/np1.png'))
      .add('np2', require('@/assets/card/np2.png'))
      .add('np3', require('@/assets/card/np3.png'))
      .add('np4', require('@/assets/card/np4.png'))
      .add('np5', require('@/assets/card/np5.png'))
      .add('arr_left', require('@/assets/icon/arr_left.png'))
      .add('arr_right', require('@/assets/icon/arr_right.png'))
      .add('arr_top', require('@/assets/icon/arr_top.png'))
      .load(() => {
        this.isHaveLoad = true
        cb && cb()
      })

  }

  initData(d, cb) {
    if (!this.isHaveLoad) {
      this.loadRes(() => {
        cb && this.renderData(d, cb)
      })
    } else {
      cb && this.renderData(d, cb)
    }

  }

  getInit() {
    this.container && this.container.destroy();
    this.container0 && this.container0.destroy();
    this.container1 && this.container1.destroy();
    this.container2 && this.container2.destroy();

    this.topcontainer1 && this.topcontainer1.destroy();
    this.topcontainer2 && this.topcontainer2.destroy();
    this.topcontainer3 && this.topcontainer3.destroy();
    this.topcontainer5 && this.topcontainer5.destroy();

    this.rightcontainer1 && this.rightcontainer1.destroy();
    this.rightcontainer2 && this.rightcontainer2.destroy();
    this.rightcontainer3 && this.rightcontainer3.destroy();
    this.rightcontainer5 && this.rightcontainer5.destroy();

    this.leftcontainer1 && this.leftcontainer1.destroy();
    this.leftcontainer2 && this.leftcontainer2.destroy();
    this.leftcontainer3 && this.leftcontainer3.destroy();
    this.leftcontainer5 && this.leftcontainer5.destroy();

  }


  renderData(d, cb) {

    // console.log(d)

    if (!d) {
      return false
    }

    //初始化自己 1
    if (d.user5Data) {
      d.user1Data && this.initMyself(d.user1Data, true)
    } else {
      d.user1Data && this.initMyself(d.user1Data)
    }

    // console.log('##############################'+JSON.stringify(d))

    this.initMyselfHaveGo(d.user1Data.out)
    this.initMyselfChi(d.user1Data.meld)


    let f1 = []
    let f2 = []
    let f3 = []
    let f4 = []
    d.user1Data.flower.forEach(v => {
      f1 = f1.concat(v.list)
    })
    d.user2Data.flower.forEach(v => {
      f2 = f2.concat(v.list)
    })
    d.user3Data.flower.forEach(v => {
      f3 = f3.concat(v.list)
    })
    d.user4Data.flower.forEach(v => {
      f4 = f4.concat(v.list)
    })

    this.initMyselfFlower(f1)
    //初始化有左边
    this.initLeft(d.user4Data.basis)
    this.initLeftHavaGo(d.user4Data.out)
    this.initLeftChi(d.user4Data.meld)
    this.initLeftFlower(f4)

    //初始化右边
    this.initRight(d.user2Data.basis)
    this.initRightHavaGo(d.user2Data.out)
    this.initRightChi(d.user2Data.meld)
    this.initRightFlower(f2)

    //初始化上边
    this.initTop(d.user3Data.basis)
    this.initTopHavaGo(d.user3Data.out)
    this.initTopChi(d.user3Data.meld)
    this.initTopFlower(f3)

    let len = d.user1Data.basis.length;
    let tempArr = [1, 4, 7, 10, 13]

    let canDo = tempArr.find(v => {
      return v == len
    })

    if (!canDo && !d.user5Data) {

      this.initEvent((name) => {
        cb && cb(name)
      })
    }

  }

  initEvent(cb) {
    let current = -1
    this.container.children.forEach((v, k) => {
      if (v.canDo) {
        v.interactive = true;
        v.buttonMode = true;
        //双击事件
        v.on('touchend', (e) => {
          this.container.children.forEach(v1 => {
            v1.x = v1.xx
          })
          if (current == v.name) {
            console.log(v.name)
            cb && cb(current)
          } else {
            current = v.name;
          }
          v.x = v.x - 20
        })
        //滑动事件
        v.on('touchendoutside', (e) => {
          console.log(v.name)
          cb && cb(v.name)
        })
      } else {

      }


    })
  }

  doPlay() {

  }


  /**
   * 丢骰子
   */


  doLoadTouzi(cb) {
    PIXI.loader
      .add('tt1', require('@/assets/touzi/1.png'))
      .add('tt2', require('@/assets/touzi/2.png'))
      .add('tt3', require('@/assets/touzi/3.png'))
      .add('tt4', require('@/assets/touzi/4.png'))
      .add('tt5', require('@/assets/touzi/5.png'))
      .add('tt6', require('@/assets/touzi/6.png'))
      .load(() => {
        this.isLoadTouzi = true
        cb && cb()
      })
  }

  //判断是否需要架子啊的
  doTouziMiddle(cb) {
    if (this.isLoadTouzi) {
      cb && cb()
    } else {
      this.doLoadTouzi(() => {
        cb && cb()
      })
    }
  }


  /**
   * 初始化最上面的
   */
  initTopHavaGo(data) {
    // data.reverse()
    if (this.topcontainer2) {
      this.topcontainer2.destroy()
    }
    this.topcontainer2 = new PIXI.Container();
    this.app.stage.addChild(this.topcontainer2);
    for (let i = 0; i < data.length; i++) {
      let sprite = new PIXI.Sprite(PIXI.loader.resources['s' + data[i]].texture);
      this.setSize(sprite, W * .065)
      sprite.anchor.set(1)
      sprite.rotation = -Math.PI / 2

      if (i >= 11) {
        sprite.x = W * .39
        sprite.y = W * .44 + W * .055 * ((i - 11) + 1)
      } else {
        sprite.x = W * .329
        sprite.y = W * .44 + W * .055 * (i + 1)
      }

      this.topcontainer2.addChild(sprite);
    }
  }

  initTopFlower(data) {
    if (this.topcontainer5) {
      this.topcontainer5.destroy()
    }
    this.topcontainer5 = new PIXI.Container();
    this.app.stage.addChild(this.topcontainer5);
    for (let i = 0; i < data.length; i++) {
      let sprite = new PIXI.Sprite(PIXI.loader.resources['s' + data[i]].texture);
      this.setSize(sprite, W * .06)
      sprite.anchor.set(1)
      sprite.rotation = -Math.PI / 2

      this.setSize(sprite, W * .07)
      sprite.x = W * .1
      sprite.y = W * .15 + W * .055 * (i + 1)
      this.topcontainer5.addChild(sprite);
    }
  }

  initTopChi(data) {

    let d = []

    for (let k = 0; k <= data.length - 1; k++) {


      if (data[k].type == '111' || data[k].type == '123') {


        let temp = {}

        if (data[k].type == '111') {

          temp = {
            info: data[k].info.slice(0, 3),
            id: data[k].mahjongId,
            playerId: data[k].playerId,
            isp: true,
            type: data[k].type
          }
        } else {
          temp = {
            info: data[k].info.slice(0, 3),
            id: data[k].mahjongId,
            playerId: data[k].playerId,
            isp: false,
            type: data[k].type
          }
        }

        d.push(temp)

      } else {
        let temp = {
          info: data[k].info,
          id: data[k].mahjongId,
          playerId: data[k].playerId,
          isp: true,
          type: data[k].type
        }
        d.push(temp)
      }

    }
    // console.log('dd' + JSON.stringify(d))
    if (this.topcontainer3) {
      this.topcontainer3.destroy()
    }
    this.topcontainer3 = new PIXI.Container();
    this.app.stage.addChild(this.topcontainer3);

    let index = -1
    for (let i = 0; i < d.length; i++) {
      for (let j = 0; j < d[i]['info'].length; j++) {
        index++
        if (j == 0) {
          index += 0.105
        }
        let sprite = null;

        if (d[i].type == 2222) {
          if (j == 3) {
            sprite = new PIXI.Sprite(PIXI.loader.resources['np3'].texture);
          } else {
            sprite = new PIXI.Sprite(PIXI.loader.resources['np3'].texture);
          }
        } else {
          sprite = new PIXI.Sprite(PIXI.loader.resources['s' + d[i]['info'][j]].texture);
        }

        this.setSize(sprite, W * .07)
        sprite.anchor.set(1)
        sprite.rotation = -Math.PI / 2

        sprite.x = W * .2
        sprite.y = W * .32 + index * W * .06


        let iconSprite = null

        if (j == 3) {
          sprite.x = W * .20
          sprite.y = W * .32 + (index - 2) * W * .06
          sprite.tint = 0x999999;
          iconSprite = this.doIcon(d[i].playerId, sprite, 3)
        }


        if (d[i]['id'] === d[i]['info'][j]) {
          if (d[i]['isp'] && j == 1) {
            if (j < 3) {
              sprite.tint = 0x999999;
              iconSprite = this.doIcon(d[i].playerId, sprite, 3)
            }

          } else if (!d[i]['isp']) {
            sprite.tint = 0x999999;
            iconSprite = this.doIcon(d[i].playerId, sprite, 3)
          }
        }
        this.topcontainer3.addChild(sprite);
        iconSprite && this.topcontainer3.addChild(iconSprite);


      }


    }


  }

  initTop(data) {

    if (this.topcontainer1) {
      this.topcontainer1.destroy()
    }
    this.topcontainer1 = new PIXI.Container();
    this.app.stage.addChild(this.topcontainer1);
    for (let i = 0; i < data.length; i++) {
      let sprite = new PIXI.Sprite(PIXI.loader.resources['np3'].texture);
      this.setSize(sprite, W * .065)
      sprite.rotation = -Math.PI / 2
      sprite.x = W * .11
      sprite.y = H - (W * .29 + W * .065 * (i + 1))
      this.topcontainer1.addChild(sprite);
    }

  }


  /**
   * 初始化右边的
   *
   */

  initRightFlower(data) {
    if (this.rightcontainer5) {
      this.rightcontainer5.destroy()
    }
    this.rightcontainer5 = new PIXI.Container();
    this.app.stage.addChild(this.rightcontainer5);
    for (let i = 0; i < data.length; i++) {
      let sprite = new PIXI.Sprite(PIXI.loader.resources['p' + data[i]].texture);
      this.setSize(sprite, W * .075)
      sprite.anchor.set(1)
      sprite.rotation = -Math.PI / 2
      sprite.x = W - (W * .25 + W * .042 * ((data.length - i) + 1))
      sprite.y = H * .155
      this.rightcontainer5.addChild(sprite);
    }
  }

  initRightHavaGo(data) {
    // data.reverse()

    if (this.rightcontainer2) {
      this.rightcontainer2.destroy()
    }
    this.rightcontainer2 = new PIXI.Container();
    this.app.stage.addChild(this.rightcontainer2);
    for (let i = 0; i < data.length; i++) {
      let sprite = new PIXI.Sprite(PIXI.loader.resources['p' + data[i]].texture);
      this.setSize(sprite, W * .075)
      sprite.rotation = -Math.PI / 2


      if (i >= 12) {
        sprite.x = W - (W * .25 + W * .04 * ((12 - i + 12) + 1))
        sprite.y = H * .28

      } else {
        sprite.x = W - (W * .25 + W * .04 * ((12 - i) + 1))
        sprite.y = H * .245

      }


      this.rightcontainer2.addChild(sprite);
    }
  }

  initRightChi(data) {


    let d = []

    for (let k = 0; k <= data.length - 1; k++) {


      if (data[k].type == '111' || data[k].type == '123') {


        let temp = {}

        if (data[k].type == '111') {

          temp = {
            info: data[k].info.slice(0, 3),
            id: data[k].mahjongId,
            playerId: data[k].playerId,
            isp: true,
            type: data[k].type
          }
        } else {
          temp = {
            info: data[k].info.slice(0, 3),
            id: data[k].mahjongId,
            playerId: data[k].playerId,
            isp: false,
            type: data[k].type
          }
        }

        d.push(temp)

      } else {
        let temp = {
          info: data[k].info,
          id: data[k].mahjongId,
          playerId: data[k].playerId,
          isp: true,
          type: data[k].type
        }
        d.push(temp)
      }

    }

    // console.log('dd' + JSON.stringify(d))
    if (this.rightcontainer3) {
      this.rightcontainer3.destroy()
    }
    this.rightcontainer3 = new PIXI.Container();
    this.app.stage.addChild(this.rightcontainer3);


    let index = -1
    let sum = 0
    d.forEach(v => {
      sum += v.info.length
    })

    for (let i = 0; i < d.length; i++) {
      for (let j = 0; j < d[i]['info'].length; j++) {
        index++
        if (j == 0) {
          index += 0.105
        }

        let sprite = null;


        if (d[i].type == 2222) {

          if (j == 3) {
            sprite = new PIXI.Sprite(PIXI.loader.resources['np5'].texture);
            sprite.rotation = -Math.PI / 2

          } else {
            sprite = new PIXI.Sprite(PIXI.loader.resources['np5'].texture);
            sprite.rotation = -Math.PI / 2
            this.setSize(sprite, W * .075)

          }

        } else {
          sprite = new PIXI.Sprite(PIXI.loader.resources['p' + d[i]['info'][j]].texture);
          sprite.rotation = -Math.PI / 2
          this.setSize(sprite, W * .075)

        }

        this.setSize(sprite, W * .075)
        // sprite.anchor.set(1)


        sprite.x = W - (W * .25 + W * .04 * (sum - index))
        sprite.y = H * .155
        let iconSprite = null

        if (j == 3) {
          sprite.x = W - (W * .25 + W * .04 * (sum - index + 2))
          sprite.y = H * .152
          sprite.tint = 0x999999;
          iconSprite = this.doIcon(d[i].playerId, sprite, 2)
        }

        if (d[i]['id'] === d[i]['info'][j]) {
          if (d[i]['isp'] && j == 1) {
            if (d[i].info.length < 4) {
              sprite.tint = 0x999999;
              iconSprite = this.doIcon(d[i]['playerId'], sprite, 2)
            }
          } else if (!d[i]['isp']) {
            sprite.tint = 0x999999;
            iconSprite = this.doIcon(d[i].playerId, sprite, 2)

          }
        }
        this.rightcontainer3.addChild(sprite);
        iconSprite && this.rightcontainer3.addChild(iconSprite);


      }


    }


  }

  initRight(data) {


    if (this.rightcontainer1) {
      this.rightcontainer1.destroy()
    }
    this.rightcontainer1 = new PIXI.Container();
    this.app.stage.addChild(this.rightcontainer1);
    for (let i = 0; i < data.length; i++) {
      let sprite = new PIXI.Sprite(PIXI.loader.resources['np2'].texture);
      sprite.rotation = -Math.PI / 2
      this.setSize(sprite, W * .075)
      sprite.x = W * .07 + W * .05 * (i + 1)
      sprite.y = H * .16
      this.rightcontainer1.addChild(sprite);
    }

  }


  /**
   *初始化桌面左边的
   */
  initLeftHavaGo(data) {
    // data.reverse()

    if (this.leftcontainer2) {
      this.leftcontainer2.destroy()
    }
    this.leftcontainer2 = new PIXI.Container();
    this.app.stage.addChild(this.leftcontainer2);
    for (let i = 0; i < data.length; i++) {
      let sprite = new PIXI.Sprite(PIXI.loader.resources['b' + data[i]].texture);
      this.setSize(sprite, W * .075)
      sprite.rotation = -Math.PI / 2

      if (i >= 12) {
        sprite.x = W * .18 + W * .042 * (i - 12 + 1)
        sprite.y = H * .765

      } else {
        sprite.x = W * .18 + W * .042 * (i + 1)
        sprite.y = H * .8

      }

      this.leftcontainer2.addChild(sprite);
    }
  }

  initLeftFlower(data) {
    if (this.leftcontainer5) {
      this.leftcontainer5.destroy()
    }
    this.leftcontainer5 = new PIXI.Container();
    this.app.stage.addChild(this.leftcontainer5);
    for (let i = 0; i < data.length; i++) {
      let sprite = new PIXI.Sprite(PIXI.loader.resources['b' + data[i]].texture);
      this.setSize(sprite, W * .075)
      sprite.rotation = -Math.PI / 2
      sprite.x = W * .18 + W * .04 * (i + 1)
      sprite.y = H * .88
      this.leftcontainer5.addChild(sprite);
    }
  }

  initLeftChi(data) {

    let d = []

    for (let k = 0; k <= data.length - 1; k++) {


      if (data[k].type == '111' || data[k].type == '123') {


        let temp = {}

        if (data[k].type == '111') {

          temp = {
            info: data[k].info.slice(0, 3),
            id: data[k].mahjongId,
            playerId: data[k].playerId,
            isp: true,
            type: data[k].type
          }
        } else {
          temp = {
            info: data[k].info.slice(0, 3),
            id: data[k].mahjongId,
            playerId: data[k].playerId,
            isp: false,
            type: data[k].type
          }
        }

        d.push(temp)

      } else {
        let temp = {
          info: data[k].info,
          id: data[k].mahjongId,
          playerId: data[k].playerId,
          isp: true,
          type: data[k].type
        }
        d.push(temp)
      }

    }

    // console.log('dd' + JSON.stringify(d))


    if (this.leftcontainer3) {
      this.leftcontainer3.destroy()
    }
    this.leftcontainer3 = new PIXI.Container();
    this.app.stage.addChild(this.leftcontainer3);


    let index = -1
    for (let i = 0; i < d.length; i++) {
      for (let j = 0; j < d[i]['info'].length; j++) {

        index++

        if (j == 0) {
          index += 0.105
        }
        let sprite = null
        if (d[i].type === 2222) {
          if (j == 3) {
            sprite = new PIXI.Sprite(PIXI.loader.resources['np5'].texture);
          } else {
            sprite = new PIXI.Sprite(PIXI.loader.resources['np5'].texture);
          }
        } else {
          sprite = new PIXI.Sprite(PIXI.loader.resources['b' + d[i]['info'][j]].texture);
        }

        this.setSize(sprite, W * .075)
        sprite.rotation = -Math.PI / 2
        sprite.x = W * .1 + W * .04 * index
        sprite.y = H * .92

        let iconSprite = null

        if (j == 3) {
          sprite.x = W * .1 + W * .04 * (index - 2)
          sprite.y = H * .917
          sprite.tint = 0x999999;
          iconSprite = this.doIcon(d[i].playerId, sprite, 4)
        }


        if (d[i]['id'] === d[i]['info'][j]) {
          if (d[i]['isp'] && j == 1) {
            if (d[i].info.length < 4) {
              sprite.tint = 0x999999;
              iconSprite = this.doIcon(d[i].playerId, sprite, 4)
            }
          } else if (!d[i]['isp']) {
            sprite.tint = 0x999999;
            iconSprite = this.doIcon(d[i].playerId, sprite, 4)

          }
        }
        this.leftcontainer3.addChild(sprite);
        iconSprite && this.leftcontainer3.addChild(iconSprite);


      }


    }


  }

  initLeft(data) {


    if (this.leftcontainer1) {
      this.leftcontainer1.destroy()
    }
    this.leftcontainer1 = new PIXI.Container();
    this.app.stage.addChild(this.leftcontainer1);
    let txt = PIXI.loader.resources['np1'].texture;
    for (let i = 0; i < data.length; i++) {
      let sprite = new PIXI.Sprite(txt);
      sprite.rotation = -Math.PI / 2
      this.setSize(sprite, W * .075)

      sprite.x = W - (W * .2 + W * .05 * (data.length - i + 1))
      sprite.y = H * .93
      this.leftcontainer1.addChild(sprite);
    }

  }


  /**
   * 初始化自己的
   * @param data
   */
  initMyselfHaveGo(data) {
    // data.reverse()


    if (this.container1) {
      this.container1.destroy()
    }
    this.container1 = new PIXI.Container();
    this.app.stage.addChild(this.container1);
    for (let i = data.length - 1; i >= 0; i--) {
      let sprite = new PIXI.Sprite(PIXI.loader.resources['j' + data[i]].texture);
      this.setSize(sprite, W * .07)
      sprite.rotation = -Math.PI / 2

      if (i >= 11) {
        sprite.x = W * .63

        sprite.y = H * .89 - (W * .3 + (i - 11) * W * .06)
      } else {
        sprite.x = W * .7

        sprite.y = H * .89 - (W * .3 + i * W * .06)
      }


      this.container1.addChild(sprite);
    }
  }

  initMyselfFlower(data) {

    if (this.container0) {
      this.container0.destroy()
    }
    this.container0 = new PIXI.Container();
    this.app.stage.addChild(this.container0);
    for (let i = 0; i < data.length; i++) {
      let sprite = new PIXI.Sprite(PIXI.loader.resources['j' + data[i]].texture);
      this.setSize(sprite, W * .075)
      sprite.rotation = -Math.PI / 2
      sprite.x = W * .76
      sprite.y = H - (W * .2 + i * W * .065)
      this.container0.addChild(sprite);
    }
  }

  initMyself(data1, isV = false) {

    if (typeof(data1.basis) == 'undefined') {
      return false
    }
    let data = [...data1.basis]

    let limitData = [...data1.playLimit]

    data.reverse()


    this.container && this.container.destroy()
    this.container = new PIXI.Container();
    this.app.stage.addChild(this.container);


    let len = data.length

    let tempArr = [1, 4, 7, 10, 13]

    let canDo = tempArr.find(v => {
      return v == len
    })


    let flag = false


    if (!data1.mahjongId && !canDo) {
      flag = true
    }

    if (data1.mahjongId && !canDo) {
      data.find((v, k) => {
        if (v == data1.mahjongId) {
          flag = true
          if (data.length < len) {
            return
          } else {
            data.splice(k, 1)
          }
        }
      })


      data.unshift(data1.mahjongId)

    }


    for (let i = 0; i < data.length; i++) {


      let sprite = {}

      if (isV) {
        sprite = new PIXI.Sprite(PIXI.loader.resources['np3'].texture);
      } else {
        sprite = new PIXI.Sprite(PIXI.loader.resources['t' + data[i]].texture);
      }

      // console.log(limitData.indexOf(data[i]))
      if (limitData.indexOf(data[i]) !== -1) {
        sprite.canDo = false
        sprite.tint = 0x999999
      } else {
        sprite.canDo = true
      }


      this.setSize(sprite, W * .11)
      sprite.rotation = -Math.PI / 2
      sprite.x = W * .85
      sprite.y = W * .12 + (i + 1) * W * .103
      sprite.xx = sprite.x
      sprite.name = data[i]
      if (i == 0 && flag) {
        sprite.y = W * .12 + (i + 1) * W * .103 - 30
      }
      this.container.addChild(sprite);

    }


  }

  initMyselfChi(data) {
    let d = []

    for (let k = 0; k <= data.length - 1; k++) {


      if (data[k].type == '111' || data[k].type == '123') {


        let temp = {}

        if (data[k].type == '111') {

          temp = {
            info: data[k].info.slice(0, 3),
            id: data[k].mahjongId,
            playerId: data[k].playerId,
            isp: true,
            type: data[k].type
          }
        } else {
          temp = {
            info: data[k].info.slice(0, 3),
            id: data[k].mahjongId,
            playerId: data[k].playerId,
            isp: false,
            type: data[k].type
          }
        }

        d.push(temp)

      } else {
        let temp = {
          info: data[k].info,
          id: data[k].mahjongId,
          playerId: data[k].playerId,
          isp: true,
          type: data[k].type
        }
        d.push(temp)
      }

    }

    // console.log('dd' + JSON.stringify(d))
    if (this.container2) {
      this.container2.destroy()
    }
    this.container2 = new PIXI.Container();
    this.app.stage.addChild(this.container2);


    let index = -1
    for (let i = 0; i < d.length; i++) {
      for (let j = 0; j < d[i].info.length; j++) {
        index++;
        if (j == 0) {
          index += 0.105
        }
        let sprite = null
        console.log(d[i].type)
        if (d[i].type == 2222) {
          if (j == 3) {
            sprite = new PIXI.Sprite(PIXI.loader.resources['np4'].texture);
          } else {
            sprite = new PIXI.Sprite(PIXI.loader.resources['np4'].texture);
          }
        } else {
          sprite = new PIXI.Sprite(PIXI.loader.resources['j' + d[i]['info'][j]].texture);
        }
        this.setSize(sprite, W * .09)


        let spriteIcon = null

        sprite.x = W * .88
        sprite.y = H - W * .08 * index
        sprite.rotation = -Math.PI / 2


        if (j == 3) {
          sprite.x = W * .86
          sprite.y = H - W * .08 * (index - 2)
          sprite.tint = 0x999999;
          spriteIcon = this.doIcon(d[i]['playerId'], sprite, 1)
        }


        if (d[i]['id'] === d[i]['info'][j]) {
          if (d[i]['isp'] && j == 1) {
            // console.log('碰' + d[i]['info'][j])
            if (d[i].info.length < 4) {
              sprite.tint = 0x999999;
              spriteIcon = this.doIcon(d[i]['playerId'], sprite, 1)
            }


          } else if (!d[i]['isp']) {
            // console.log('吃'+d[i]['info'][j])
            sprite.tint = 0x999999;
            spriteIcon = this.doIcon(d[i]['playerId'], sprite, 1)
          }
        } else {
        }

        this.container2.addChild(sprite);
        spriteIcon && this.container2.addChild(spriteIcon);

      }
    }
  }

  /**
   *
   * @param id 用户id
   * @param sprite 需要图标的
   * @param site 哪一家
   */
  doIcon(id, sprite, site) {
    let members = store.getters.members;
    let all_users = store.getters.all_users;
    let myPos = null
    if (site == 1) {
      myPos = all_users.user1Data.position
    } else if (site == 2) {
      myPos = all_users.user2Data.position
    } else if (site == 3) {
      myPos = all_users.user3Data.position
    } else if (site == 4) {
      myPos = all_users.user4Data.position
    }


    let otherPos = members.find(v => {
      if (v.PlayerId == id) {
        return v.position;
      }
    })

    if (!otherPos) {
      return false
    }
    if (otherPos.position == myPos) {
      return false
    }


    // console.log('otherPos' + JSON.stringify(otherPos))
    // console.log('myPos' + myPos)
    // console.log(id, sprite, site)


    let iconSprite = null
    if (otherPos.position - myPos == 1 || otherPos.position - myPos == -3) {
      iconSprite = new PIXI.Sprite(PIXI.loader.resources['arr_right'].texture);
    } else if (Math.abs(otherPos.position - myPos) == 2) {
      iconSprite = new PIXI.Sprite(PIXI.loader.resources['arr_top'].texture);
    } else if (myPos - otherPos.position == 1 || myPos - otherPos.position == -3) {
      iconSprite = new PIXI.Sprite(PIXI.loader.resources['arr_left'].texture);
    } else {

    }

    if (site == 1) {
      iconSprite.pivot.x = .5
      iconSprite.pivot.y = .5
      this.setSize(iconSprite, W * .04)
      iconSprite.x = sprite.x + 20
      iconSprite.y = sprite.y - 20
      iconSprite.zOrder = 1000
      iconSprite.rotation = -Math.PI / 2
    } else if (site == 2) {
      this.setSize(iconSprite, W * .03)
      iconSprite.x = sprite.x + 28
      iconSprite.y = sprite.y - 15
      iconSprite.zOrder = 1000
      iconSprite.rotation = Math.PI
    } else if (site == 3) {
      this.setSize(iconSprite, W * .035)
      iconSprite.x = sprite.x - 30
      iconSprite.y = sprite.y + 10
      iconSprite.zOrder = 1000
      iconSprite.rotation = Math.PI / 2
    } else if (site == 4) {
      this.setSize(iconSprite, W * .03)
      iconSprite.x = sprite.x + 10
      iconSprite.y = sprite.y - W * .05
      iconSprite.zOrder = 1000
    }

    return iconSprite


  }

  setSize(sprite, len, size = 'width') {
    if (size === 'height') {
      let precent = len / sprite.height
      sprite.height = len
      sprite.width = sprite.width * precent
    } else {
      let precent = len / sprite.width
      sprite.width = len
      sprite.height = sprite.height * precent
    }

  }

}
