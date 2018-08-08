/**
 * Created by LISHI on 2017/4/27.
 */
/**
 * Created by LISHI on 2017/4/25.
 */
import * as types from '../mutation-types'
import brg from '../../utils/middle'
const state = {
  user: {
   /* id:1,*/
  },
  status: '',
  game_user: [
    // {
    //   PlayerId:1,
    //   playerName:1111111,
    //   position:1,
    //   iconUrl:"https://www.baidu.com/img/bd_logo1.png"
    // },
    // {
    //   PlayerId:2,
    //   playerName:222222,
    //   position:2,
    //   iconUrl:"https://www.baidu.com/img/bd_logo1.png"
    // },
    // {
    //   PlayerId:3,
    //   playerName:33333,
    //   position:3,
    //   iconUrl:"https://www.baidu.com/img/bd_logo1.png"
    // },
    // {
    //   PlayerId:4,
    //   playerName:444444,
    //   position:4,
    //   iconUrl:"https://www.baidu.com/img/bd_logo1.png"
    // }
  ],
  roomId: 1,
  ownerId:0,
  allUser: {
   /* user1Data:{
      basis:[34,13,13,14,14,14,13,43],
      flag:0,
      iconUrl:"https://www.baidu.com/img/bd_logo1.png",
      id:"100000002",
      mahjongId:0,
      out:[],
      position:1,
      state:3,
      type:1,
      leftTime:10,
      playLimit:[34,13],
      canmeld:[
        {
          type:'123',
          playerId:2,
          mahjongId:23,
          info:[22,23,24]
        },
        {
          type:'1111',
          playerId:2,
          mahjongId:23,
          info:[24,24,24,24]
        },
        {
          type:'2222',
          playerId:2,
          mahjongId:23,
          info:[24,24,24,24]
        }
      ],
      meld:[
        {
          type:1111,
          playerId:4,
          mahjongId:11,
          info:[11,11,11,11]
        },
        {
          type:2222,
          playerId:3,
          mahjongId:22,
          info:[22,22,22,22]
        }
      ],
      flower:[
        {
          type:true,
          list:[1,2,3,4]
        }
      ],

    },
    user2Data:{
      basis:[34,13,13,14,14,14,13,43],
      flag:0,
      iconUrl:"https://www.baidu.com/img/bd_logo1.png",
      id:"100000002",
      mahjongId:0,
      out:[],
      position:1,
      state:3,
      type:1,
      leftTime:10,
      playLimit:[34,13],
      canmeld:[
        {
          type:'123',
          playerId:2,
          mahjongId:23,
          info:[22,23,24]
        },
        {
          type:'1111',
          playerId:2,
          mahjongId:23,
          info:[24,24,24,24]
        },
        {
          type:'2222',
          playerId:2,
          mahjongId:23,
          info:[24,24,24,24]
        }
      ],
      meld:[
        {
          type:1111,
          playerId:4,
          mahjongId:11,
          info:[11,11,11,11]
        },
        {
          type:2222,
          playerId:3,
          mahjongId:22,
          info:[22,22,22,22]
        }
      ],
      flower:[
        {
          type:true,
          list:[1,2,3,4]
        }
      ],

    },
    user3Data:{
      basis:[34,13,13,14,14,14,13,43],
      flag:0,
      iconUrl:"https://www.baidu.com/img/bd_logo1.png",
      id:"100000002",
      mahjongId:0,
      out:[],
      position:1,
      state:3,
      type:1,
      leftTime:10,
      playLimit:[34,13],
      canmeld:[
        {
          type:'123',
          playerId:2,
          mahjongId:23,
          info:[22,23,24]
        },
        {
          type:'1111',
          playerId:2,
          mahjongId:23,
          info:[24,24,24,24]
        },
        {
          type:'2222',
          playerId:2,
          mahjongId:23,
          info:[24,24,24,24]
        }
      ],
      meld:[
        {
          type:1111,
          playerId:4,
          mahjongId:11,
          info:[11,11,11,11]
        },
        {
          type:2222,
          playerId:3,
          mahjongId:22,
          info:[22,22,22,22]
        }
      ],
      flower:[
        {
          type:true,
          list:[1,2,3,4]
        }
      ],

    },
    user4Data:{
      basis:[34,13,13,14,14,14,13,43],
      flag:0,
      iconUrl:"https://www.baidu.com/img/bd_logo1.png",
      id:"100000002",
      mahjongId:0,
      out:[],
      position:1,
      state:3,
      type:1,
      leftTime:10,
      playLimit:[34,13],
      canmeld:[
        {
          type:'123',
          playerId:2,
          mahjongId:23,
          info:[22,23,24]
        },
        {
          type:'1111',
          playerId:2,
          mahjongId:23,
          info:[24,24,24,24]
        },
        {
          type:'2222',
          playerId:2,
          mahjongId:23,
          info:[24,24,24,24]
        }
      ],
      meld:[
        {
          type:1111,
          playerId:4,
          mahjongId:11,
          info:[11,11,11,11]
        },
        {
          type:2222,
          playerId:3,
          mahjongId:22,
          info:[22,22,22,22]
        }
      ],
      flower:[
        {
          type:true,
          list:[1,2,3,4]
        }
      ],

    },*/
  },
  reconnect_key:''
}
const mutations = {
  [types.LOGIN](state) {
    state.isLogin = true
  },
  [types.FETCH_OWNER_ID](state,id) {
    state.ownerId = id
  },
  [types.FETCH_USER_INFO](state, data) {
    state.user = data
  },
  [types.FETCH_MEMBERS_INFO](state, data) {
    state.game_user = data
  },
  [types.FETCH_ROOM_ID](state, data) {
    state.roomId = data
  },
  [types.FETCH_REC_KEY](state, data) {
    state.reconnect_key = data
  },


  [types.FETCH_ALL_USER](state, data) {
    state.allUser = Object.assign({},state.allUser,data)
    brg.$emit('ak486', state.allUser)


  },
  [types.DELETE_USER](state) {
    state.allUser = {}
  },
  [types.FETCH_ALL_USER_BY_ID](state, data) {
    for (let index in state.allUser) {
      if (state.allUser[index].position === data.position) {
        state.allUser[index] = data
      }


    }

    brg.$emit('ak486', state.allUser)

  }


}
const getters = {
  is_Login: state => state.isLogin,
  user_info: state => state.user,
  members: state => state.game_user,
  room_id: state => state.roomId,
  all_users: state => state.allUser,
  ownerId: state => state.ownerId,
  reconnect_key: state => state.reconnect_key,
}


const actions = {
  login({commit}) {
    commit(types.LOGIN)

  },
  initUserState({commit},){
    commit(types.FETCH_OWNER_ID, 0)
    commit(types.DELETE_USER)
    commit(types.FETCH_MEMBERS_INFO, [])
    commit(types.FETCH_ROOM_ID, 1)
  },
  fetchUserInfo({commit}, data){
    commit(types.FETCH_USER_INFO, data)
  },
  fetchMembers({commit}, data){
    commit(types.FETCH_MEMBERS_INFO, data)
  },
  fetchRoomId({commit}, data){
    commit(types.FETCH_ROOM_ID, data)
  },

  fetchAllUser({commit}, data){
    console.log(data)
    commit(types.FETCH_ALL_USER, data)

  },
  deleteAllUser({commit}){
    commit(types.DELETE_USER)
  },
  fetchOwnerId({commit}, data){
    commit(types.FETCH_OWNER_ID, data)
  },

  fetchAllUserById({commit}, data){
    commit(types.FETCH_ALL_USER_BY_ID, data)
  },
  fetchReconnectKey({commit}, data){
    commit(types.FETCH_REC_KEY, data)
  }

}


export default {
  state,
  mutations,
  getters,
  actions
}
