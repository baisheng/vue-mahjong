/**
 * Created by LISHI on 2017/4/25.
 */
import * as types from '../mutation-types'


let touziTimer = null
let typeTimer = null


const state = {
  initData: {},
  userState: [],
  isShowZhanji: false,
  isShowMsg: false,
  showCount: {

    /*data: {
      isWin: true,
      combinationList: [{
        info: [21, 22, 23]
      },
        {
          info: [21, 22, 23]
        },
        {
          info: [21, 22, 23]
        },
        {
          info: [21, 22, 23]
        }],
      list: [
        {
          flag: 3,
          id: 1,
          type:2,
          reward: [{
            num: 10,
            type: 1
          }]
        },
        {
          flag: 4,
          id: 2,
          type:2,
          reward: [{
            num: 5,
            type: 1
          }]
        },
        {
          flag: 4,
          id: 3,
          type:1,
          reward: [{
            num: 5,
            type: 1
          }]
        }, {
          flag: 0,
          id: 4,
          type:0,
          reward: [{
            num: 0,
            type: 1
          }]
        }

      ],

      typeList: [{
        score: 13,
        type: 14
      }, {
        score: 1,
        type: 13
      }],
    },

    show: false*/
  },
  showCard: {
    card: 1,
    site: 1,
    show: 0
  },
  cardType: {
    data: [
       /* {
          type:'123',
          playerId:2,
          mahjongId:23,
          info:[22,23,24]
        },
        {
          type:'123',
          playerId:2,
          mahjongId:23,
          info:[24,25,26]
        },
        {
          type:'1111',
          playerId:2,
          mahjongId:23,
          info:[23,23,23,23]
        }*/
           // {
           //   type: '123',
           //   playerId: 1001,
           //   mahjongId: '23',
           //   info: [21, 22, 23]
           // },
           // {
           //   type: '111',
           //   playerId: 1001,
           //   mahjongId: '23',
           //   info: [23, 23, 23]
           // },
           // {
           //   type: '1111',
           //   playerId: 1001,
           //   mahjongId: '23',
           //   info: [23, 23, 23]
           // }
    ],
    show: false,
    ishu: false
  },
  sharetip: {
    data: {},
    show: false
  },
  touzistatus: {
    point: 10,
    show: false,
  },
  showstart: {
    show: false,
    longTime:''
  },
  bankerSite: 0,
  showSite: {
    show: false,
    position: 0
  },
  loading: {
    show: true
  },
  room_status: {
    roomId: 0,
    flag: 0
  },
  //积分
  record: [
    // {
    //   playerId:1,
    //   flag:true,
    //   score:33
    // },
    // {
    //   playerId:2,
    //   flag:false,
    //   score:33
    // },
    // {
    //   playerId:3,
    //   flag:true,
    //   score:33
    // },
    // {
    //   playerId:4,
    //   flag:false,
    //   score:33
    // }

  ],
  connectStatus: {
    status: false,
    isOn: false,
    msg: '连接中',
    show: false
  }

}
const mutations = {

  [types.FETCH_DATA](state, data) {
    state.initData = data
  },
  [types.FETCH_USER_STATE_DATA](state, data) {
    state.userState = data
  },
  [types.SHOW_ZHANJI](state, data) {
    state.isShowZhanji = data
  },
  [types.SHOW_ROOM_MSG](state, data) {
    state.isShowMsg = data
  },
  [types.SHOW_COUNT](state, data) {
    state.showCount = data
  },

  [types.SHOW_SINGLE_CARD](state, data) {
    state.showCard = Object.assign({},state.showCard,data)
  },
  [types.FETCH_TYPE](state, data) {
    state.cardType = data
  },
  [types.SHOW_SHARE_TIP](state, data) {
    state.sharetip = data
  },
  [types.SHOW_TOUZI](state, data) {
    state.touzistatus = data
  },
  [types.SHOW_START](state, data) {
    state.showstart = Object.assign({},state.showstart,data)
  },
  [types.FETCH_BANKER](state, data) {
    state.bankerSite = data
  },
  [types.SHOW_SITE](state, data) {
    state.showSite = data
  },
  [types.SHOW_LOADING](state, data) {
    state.loading = data
  },
  [types.FETCH_POINT](state, data) {
    state.record = data
  },
  [types.FETCH_ROOM_STATUS](state, data) {
    state.room_status = data
  },
  [types.FETCH_CONNECT_STATUS](state, data) {
    state.connectStatus = data
  },


}
const getters = {
  initData: state => state.initData,
  userState: state => state.userState,
  isShowZhanji: state => state.isShowZhanji,
  isShowMsg: state => state.isShowMsg,
  showCount: state => state.showCount,
  showCard: state => state.showCard,
  cardType: state => state.cardType,
  sharetip: state => state.sharetip,
  touzistatus: state => state.touzistatus,
  showstart: state => state.showstart,
  bankerSite: state => state.bankerSite,
  showSite: state => state.showSite,
  loading: state => state.loading,
  record: state => state.record,
  room_status: state => state.room_status,
  connectStatus: state => state.connectStatus,

}
const actions = {
  //退出房间清空房间数据
  initGameState({commit}){
    commit(types.SHOW_ZHANJI, false)
    commit(types.SHOW_ROOM_MSG, false)
    commit(types.SHOW_COUNT, {data:[],show:false})
    commit(types.SHOW_SINGLE_CARD, {
      card: 1,
      site: 1,
      show: 0
    },)
    commit(types.FETCH_TYPE, {
      data:[],
      show: false,
      ishu: false
    })
    commit(types.SHOW_START, {
      show: false,
      longTime:''
    })
    commit(types.FETCH_BANKER, 0)
    commit(types.SHOW_SITE, {
      show: false,
      position: 0
    })
    commit(types.SHOW_LOADING, true)
    commit(types.FETCH_POINT, [])
    commit(types.FETCH_ROOM_STATUS, {
      roomId: 0,
      flag: 0
    })
  },

  //显示战绩
  showZj({commit}, parmas) {
    commit(types.SHOW_ZHANJI, parmas)
  },
  //显示房间信息
  showRMsg({commit}, parmas) {
    commit(types.SHOW_ROOM_MSG, parmas)
  },
  //显示结算
  showCount({commit}, parmas) {
    commit(types.SHOW_COUNT, parmas)
  },
  //出牌
  showCard({commit}, parmas) {
    commit(types.SHOW_SINGLE_CARD, parmas)
  },
  //出牌提示
  fetchCardType({commit}, parmas) {
    commit(types.FETCH_TYPE, parmas)
  },
  //显示分享信息
  showShareModal({commit}, parmas) {
    commit(types.SHOW_SHARE_TIP, parmas)
  },
  //显示点数
  showTouzi({commit}, params) {
    commit(types.SHOW_TOUZI, params)
    touziTimer && clearTimeout(touziTimer)
    touziTimer = setTimeout(() => {
      commit(types.SHOW_TOUZI, {point: 2, show: false})
      clearTimeout(touziTimer)
    }, 3500)
  },
  //显示开始
  showStart({commit}, parmas) {
    commit(types.SHOW_START, parmas)
  },

  //获取庄家位置
  fetchBanker({commit}, parmas) {
    commit(types.FETCH_BANKER, parmas)
  },
  //显示中间的位置

  showMidSite({commit}, params) {
    commit(types.SHOW_SITE, params)
  },
  showLoading({commit}, params) {
    commit(types.SHOW_LOADING, params)
  },
  //积分de
  fetchRecord({commit}, params) {
    commit(types.FETCH_POINT, params)
  },
  //获取房间状态
  fetchRoomStatus({commit}, params) {
    commit(types.FETCH_ROOM_STATUS, params)
  },

  //获取链接状态
  fetchConnectStatus({commit}, params) {
    commit(types.FETCH_CONNECT_STATUS, params)
  }


}


export default {
  state,
  mutations,
  getters,
  actions
}
